module Simplifier =
struct

open Term
open Logicterm

  class simpError s ts =
    object (self)
    inherit Result.error s
    val trms = (ts :term list)
    method get() = trms
    method print st = 
      Format.open_box 0; 
      print_string "Simplifier Error: ";
      print_string ((self#msg())^" "); 
      Format.print_newline();
      Format.open_box 0; 
      Corepp.list_print (print_term st) 
	(fun _ -> Format.print_string ","; 
	  Format.print_break 1 2; 
	  Format.close_box(); Format.open_box 0)
	(self#get());
      Format.close_box();
      Format.close_box();
  end

  let error s t = Result.mkError((new simpError s t):>Result.error)
  let addError s t e =
    Result.addError e (error s t) 

(* simp_rule :
   variable , optional condition, lhs , rhs , source of rule
*)

type rule = 
    (binders list 
       * term option * term * term 
       * Logic.rr_type)

(* data_set:
   set of simplifier rules 
*)

(*
type data_set = 
{ 
  basic: rule list;        (* unconditonal rules *)
  conditional: rule list     (* conditional rules *)
} 

let empty_set() = { basic=[]; conditional = []}

let lookup t rls = rls
let enter p (l, b) rls = b::rls
*)

(*
type data_set = 
{ 
  basic: rule Net.net;        (* unconditonal rules *)
  conditional: rule Net.net     (* conditional rules *)
} 
*)

type data_set = 
{ 
  cond_depth: int; (* maximum number of conditions at one go *)
  assumptions: rule Net.net;   (* rules from a sequent assumption *)
  basic: rule Net.net          (* global rules *)
} 


let empty_set() = 
  { cond_depth=5; assumptions=Net.empty(); basic=Net.empty()}

let make_entry t src=
  let (qs, t1)=strip_qnt (Basic.All) t  (* get leading quantifiers *)
  in 
  if (is_equal t1)                      (* deal with simple equalities *)
  then 
    (let (l, r) = dest_equal t1 
    in 
    (l,(qs, None, l, r, src)))
  else 
    if (is_implies t1)                 (* deal with conditional equalities *)
    then 
      let (_, args)=dest_fun t1
      in 
      match args with
	[a; c] -> 
	  (if (is_equal c)
	  then 
	    let (l, r)=dest_equal c
	    in (l, (qs, Some(a), l, r, src))
	  else 
	    raise (Failure "Can't add rule: not an equality or a conditional equality\n"))
      |	_  -> raise (Failure "Can't add rule: not an equality or a conditional equality\n")
    else
      raise (Failure "Can't add rule: not an equality or a conditional equality\n")


(*
let add_simp_rule set t src =
  let (l, (vs, c, a, b, r))= make_entry t src
  in
  match c with 
    None -> 
      {basic=Net.enter 
	 (Rewrite.is_free_binder vs) (l, (vs, c, a, b, r)) set.basic;
       conditional = set.conditional}
  | Some(_) ->
      { basic=set.basic;
	conditional = 
	Net.enter 
	(Rewrite.is_free_binder vs) (l, (vs, c, a, b, r)) set.conditional}
*)

let add_simp_rule set t src =
  let (l, (vs, c, a, b, r))= make_entry t src
  in 
  let varp x= Rewrite.is_free_binder vs x
  and lt (_, _, _, x, _) (_, _, _, y, _)=Term.term_gt y x
  in
  {cond_depth=set.cond_depth;
   assumptions=set.assumptions; 
   basic=Net.insert lt varp (l, (vs, c, a, b, r)) set.basic}

let dec_cond_depth set=
  if(set.cond_depth=0) then set
  else
    {cond_depth=(set.cond_depth -1);
     assumptions=set.assumptions;
     basic=set.basic}

(* simplifier actions:

   simple actions (no conditional rewrites):
   recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied

always:
   recurse through the structure of a term, 
   getting a list of the rewrite rules to be applied
for each subterm:
   get a (un)conditional rule which could be applied
   if conditional, try to show the condition=true 
      discarding the rule on failure
   if successfull apply the rule.
*)

(* utility functions *)
(* strip_rrs: prepare for direct rewriting of term *)
(* for tests only  *)

  let strip_rrs rrs=
    let strip_rr x = 
      match x with
       Logic.RRThm x -> Formula.dest_form (Logic.dest_thm x)
      |	 _ -> failwith "simp_term"
    in 
    (List.map strip_rr rrs)


(* match_rewrite:
   try to match lhs with trm, return rhs if sucessful
*)

  let match_rewrite scp tyenv tenv varp lhs rhs trm = 
    let find_match_rewrite()=
      try 
	Unify.unify_fullenv_rewrite scp tyenv tenv varp lhs trm
      with x -> 
	failwith "Can't match terms"
    in 
    try
      (ignore(find_match_rewrite ());
       Term.subst_env tenv rhs)
    with x -> (failwith "match_rewrite")

exception No_change

(* simp_term: toplevel for term simplifier (using Rewriting directly)
   (this is not a tactic)
*)

let is_true t = Term.is_true t

let rec simp_term scp set t=

(* 
   find_rrs: scope -> Term.substitution -> data_set -> term
   make list of rewrite rules using unification
   Term.substitution is used to keep track of changing term
   as rewrites are found
*)

(* prove_condition: try to prove the condition is true
   by applying simplification rules
*)

  let rec prove_condition scp set qs tenv cnd = 
    if(set.cond_depth=0) then raise No_change
    else
      (match cnd with
	None -> ()
      | Some(cd) -> 
	  let varp=Rewrite.is_free_binder qs
	  in 
	  let c=Term.subst_env tenv cd
	  in 
	  if (is_true c) then ()
	  else 
	    (let nt=simp_term scp (dec_cond_depth set) c
	    in 
	    if(is_true nt) then ()
	    else raise No_change))
  in 

(* find_match: find rules which match a term *)

  let rec find_match scp tyenv rslt set trm =
    let chng=ref false
    in 
    let rec find_basic_aux rls t= 
      match rls with
	[] -> t
      | ((qs, c, lhs, rhs, thm)::nxt) ->
	  (try 
	    let tenv = Term.empty_subst ()
	    in 
	    let nt=
	      match_rewrite scp tyenv tenv
		(Rewrite.is_free_binder qs) lhs rhs t
	    in 
	    prove_condition scp set qs tenv c;

(*	    rslt:=thm::(!rslt); *)

	    rslt := (qs, Logicterm.mkequal lhs rhs)::(!rslt);
	    chng:=true; nt
	  with _ -> find_basic_aux nxt t)
    in 
    let nb=find_basic_aux (Net.lookup trm set.basic) trm
    in 
    if (!chng) 
    then find_match scp tyenv rslt set nb
    else nb

(* find_basic_match scp tyenv set trm rslt*) 

  in
  let rec find_rrs scp tyenv set trm =
    let rslt=ref[]
    in 
    let rec find_rrs_aux t=
      match t with
	Term.Qnt(q, b) -> 
	  (let nb = find_rrs_aux b
	  in 
	  find_match scp tyenv rslt set (Qnt(q, nb)))
      | Term.Typed(tt, ty) -> 
	  find_rrs_aux tt
      | Term.App(f, a)->
	  let nf = 
	    (find_rrs_aux f)
	  in 
	  let na = (find_rrs_aux a)
	  in 
	  (find_match scp tyenv rslt set (Term.App(nf, na)))
      | _ -> (find_match scp tyenv rslt set t)
    in
    let nt=find_rrs_aux trm
    in List.rev(!rslt)

(*
   make_rr_list: Gtypes.scope -> data_set -> term -> rr_type list
   make a list of rewrites with which to simplify the term
*)
  in 
  let rrs=
    find_rrs scp (Gtypes.empty_subst()) set t
  in 
  if rrs=[]
  then raise No_change
  else 
    (try
      Rewrite.rewrite_eqs scp true rrs t
    with _ -> raise No_change)

(* tests *)


let simp_set = ref(empty_set())

(* add_simp f: add theorem thm to simp_set *)

let add_simp thm = 
  simp_set:=add_simp_rule (!simp_set) 
      (Formula.dest_form (Logic.dest_thm thm)) (Logic.RRThm thm)

(* empty_simp: empty simp set *)
let empty_simp () = simp_set:=empty_set()

end

open Simplifier

let saxiom str = 
  Logic.mk_axiom 
	  (Formula.form_of_term (Tpenv.typenv()) (Tpenv.read str))

let scope () = Tpenv.typenv();;

let axioms = 
[
"!x: (true and x) = x";
"!x: (x and true) = x";

"!x: (false and x) = false";
"!x: (x and false) = false";

"!x: (true or x) = true";
"!x: (x or true) = true";
"!x: (false or x) = x";
"!x: (x or false) = x";

"(not false) = true";
"(not true) = false";
"!x: (not (not x))=x";
"!x y: (x implies y) = ((not x) or y)";

"(!x: true) = true";
"(!x: false) = false";

"! x: (x=x)=true"
] 

let setup()=
  List.iter (fun x-> add_simp (saxiom x)) axioms


let t=read "!x y : (not (false and x)) => (x or (y or true))";;
let t1=read "!x:(false and x)";;
let t2= read "not (not false)";;
let t3=read "!f g: 1 = 2";;

add_simp (saxiom "!f g x: true => (1=2)");;
let rrs() = List.map read axioms;;
(*
add_simp (saxiom "!x y: (x=y) = true");;
add_simp (saxiom "!f g x y: (x=y) => ((f x)=(g y))");;
*)
(*
let tyenv=Gtypes.empty_subst();;
let rrs s=[make_entry (read s) (Logic.RRThm (saxiom s))];;

add_simp (saxiom "!x: (false and x) = x");;
let rslt=ref[];;
let (q, b) = match t1 with (Term.Qnt(a, c)) -> a,c;;
let ls = Net.lookup b (!simp_set).basic;;
let (qs, c, lhs, rhs, thm)=List.hd ls;;
let varp = Rewrite.is_free_binder qs;;
*)
