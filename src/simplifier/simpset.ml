(*-----
 Name: simpset.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Basic
open Term
open Logicterm
open Simputils

(* simpsets *)

(** [rule] :
   variables, optional condition, lhs , rhs , source of rule
 *)
type rule = 
    (binders list 
       * term option * term * term 
       * Logic.rr_type)

let dest_rule (vs, cnd, lhs, rhs, rr)=(vs, cnd, lhs, rhs, rr)
let rule_binders (vs, cnd, lhs, rhs, rr)=vs
let rule_cond (vs, cnd, lhs, rhs, rr)=cnd
let rule_lhs (vs, cnd, lhs, rhs, rr)=lhs
let rule_rhs (vs, cnd, lhs, rhs, rr)=rhs
let rule_src (vs, cnd, lhs, rhs, rr)=rr

(** 
   [simpset]:
   set of simplifier rules 
 *)
type simpset = 
    { 
      convs: Logic.conv Net.net;
      basic: rule Net.net;          (* global rules *)
      next: simpset option             (* next simpset *)
    } 

let empty_set() = 
  {
   convs = Net.empty();
   basic=Net.empty(); 
   next=None
  }


(** [termnet_lt varp x y]:

   Less than ordering of terms for use with Net.insert. Makes
   variables (for which [varp] is true) larger than any other term.
 *)

let rec lt_var lvarp rvarp x y=
  let (x_is_var, y_is_var) = (lvarp x, rvarp y)
  in 
  match x_is_var, y_is_var with
    (false, true) -> true
  | (true, _) -> false
  | _ -> term_lt lvarp rvarp x y
and term_lt lvarp rvarp t1 t2 = 
  let atom_lt (a1, ty1) (a2, ty2) =  a1<a2
  and bound_lt (q1, n1, _) (q2, n2, _) =  n1<n2 & q1<q1
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> lt_var lvarp rvarp trm t2
  | _, Typed (trm, _) -> lt_var lvarp rvarp t1 trm
  | (Const c1, Const c2) -> Basic.const_lt c1 c2
  | (Const _ , _ ) -> true
  | (Id _, Const _) -> false
  | (Id _, Id _) -> atom_lt (dest_var t1) (dest_var t2)
  | (Id _, _) -> true
  | (Bound _, Const _) -> false
  | (Bound _, Id _) -> false
  | (Bound b1, Bound b2) -> bound_lt (dest_binding b1) (dest_binding b2)
  | (Bound _ , _ ) -> true
  | (Free _, Const _) -> false
  | (Free _, Id _) -> false
  | (Free _, Bound _) -> false
  | (Free (n1, _), Free (n2, _)) -> n1<n2
  | (Free _, _) -> true
  | (App _, Const _) -> false
  | (App _, Id _) -> false
  | (App _, Bound _) -> false
  | (App _, Free _) -> false
  | (App(f1, a1), App (f2, a2)) -> 
      if lt_var lvarp rvarp f1 f2 then true
      else if lt_var lvarp rvarp f2 f1 then false
      else lt_var lvarp rvarp a1 a2
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Id _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Free _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(qnt1, q1, b1), Qnt(qnt2, q2, b2)) ->
      if (lt_var lvarp rvarp b1 b2) 
      then (bound_lt (dest_binding q1) (dest_binding q2))
      else false
and 
 termnet_lt x y = 
  let lvarp = is_variable (rule_binders x)
  and rvarp = is_variable (rule_binders y)
  in 
  lt_var lvarp rvarp (rule_lhs y) (rule_lhs x)


(** [add_rule rl s]:

   Add rule [rl= c=>(l=r)] to set [s]

   If rule could lead to looping ([Simpconvs.equals_upto_vars l
   r] is true) then make rule an ordered rewrite 
   (not implemented, currently [add_rule] just prints a warning and 
   returns the set [s]).
 *)


let get_rr_order rr=
  match rr with
    Logic.OAsm(_, p) -> p
  | Logic.ORRThm(_, p) -> p
  | _ -> raise (Invalid_argument "get_rr_order")

let set_rr_order rr order=
  match rr with
    Logic.OAsm(t, p) -> Logic.OAsm(t, order)
  | Logic.ORRThm(t, p) -> Logic.ORRThm(t, order)
  | Logic.Asm(t) -> Logic.OAsm(t, order)
  | Logic.RRThm(t) -> Logic.ORRThm(t, order)

let add_rule rl s=
  let (vs, cond, l, r, src)=rl
  in 
  let varp = is_variable vs
  in 
  if(Simputils.equal_upto_vars (Rewrite.is_free_binder vs) l r)
  then 
    let order =
      try 
	get_rr_order src
      with _ -> Term.term_lt
    in 
    let rl1=(vs, cond, l, r, set_rr_order src order)
    in 
    { s with basic=Net.insert termnet_lt varp (s.basic) l rl1 }
  else 
    { s with basic=Net.insert termnet_lt varp (s.basic) l rl }

let add_conv (vars, key) conv s =
  let varp = is_variable vars 
  in 
  { s with convs=Net.add varp (s.convs) key conv }


(* [dest_rr_rule trm]: 
   Split rule [trm] into binders, condition, lhs, rhs 
   rules are of the form:
   [c=>(l=r)] or [l=r]
 *)
    let dest_rr_rule trm =
      (* Get leading quantifiers *)
      let (qs, t1)=Term.strip_qnt Basic.All trm 
      in 
      (* test for conditional equalities *)
      let (cnd, rl)=
	if (Logicterm.is_implies t1)         
	then (* is conditional *)
	  (let (_, asm, cncl)=Term.dest_binop t1
	  in 
	  (Some(asm), cncl))
	else  (* is not conditional *)
	  (None, t1)
      in 
      (* break the equality *)
      if (Logicterm.is_equality rl)
      then 
	let (lhs, rhs)=Logicterm.dest_equality rl
	in (qs, cnd, lhs, rhs)
      else 
	  raise (Failure 
		   ("Not an equality or a conditional equality\n"))


(** [make_rule rl src]:
   make rule from theorem or assumption [src] in scope [scp]
*)
    let make_rule rl trm=
      let qs, c, l, r=dest_rr_rule trm
      in 
      (qs, c, l, r, rl)

(** [lookup trm s]:

   find list of possible matches for term [trm] in set [s].
 *)
let rec find_first f l =
  match l with
    [] -> failwith "find_first"
  | (x::xs) -> try (f x) with _ -> find_first f xs

let lookup_conv scp set trm =
  let conv_list = Net.lookup set.convs trm
  in 
  try 
    let thm = find_first (fun conv -> conv scp trm) conv_list
    in 
    let (qs, conc, lhs, rhs, src) = 
      make_rule thm (Logic.term_of thm)
    in 
    [(qs, conc, lhs, rhs, Logic.RRThm(src))]
  with _ -> raise Not_found

let rec lookup scp set trm =
  try 
    lookup_conv scp set trm
  with Not_found -> 
    (try
      Net.lookup set.basic trm 
    with Not_found ->
      (match set.next with 
	None -> raise Not_found
      | Some(s) -> lookup scp s trm))


(** [join s t]
   Join sets s and t together.
   In the set [join s t], the set [s] will be searched before set [t]
 *)
let rec join s1 s2 = 
  match s1.next with 
    None ->
      {s1 with next=Some(s2)}
  | Some x -> 
      {s1 with next=Some(join x s2)}

(** [split s]
   Split simpset [s] into two parts.
   fails if [s] is not joined to another set.
 *)
let split s1=
  match s1.next with 
    None ->
      raise (Failure "split")
  | Some x -> 
      ({ s1 with next=None}, x)
	



(* Printer for simpsets *)

(* Printer for nets in a simpset *)
let print_rule ppinfo (vars, cond, lhs, rhs, src)=
  let trm = 
    Drule.rebuild_qnt Basic.All vars
      (match cond with
	None -> (Logicterm.mk_equality lhs rhs)
      | Some c -> (Logicterm.mk_implies c (Logicterm.mk_equality lhs rhs)))
  in 
  Format.printf "@[";
  (match src with
    Logic.Asm _ -> 
      Format.printf "Assumption: ";
      Term.print ppinfo trm;
  | Logic.OAsm _ -> 
      Format.printf "Ordered assumption: ";
      Term.print ppinfo trm;
  | Logic.RRThm thm -> 
      Format.printf "Theorem: ";
      Logic.print_thm ppinfo thm
  | Logic.ORRThm (othm, _) -> 
      Format.printf "Orderd theorem: ";
      Logic.print_thm ppinfo othm);
  Format.printf "@]"

let print_rule_net ppinfo net=
  let rule_printer r = print_rule ppinfo r; Format.printf "@,"
  in 
  Format.printf "@[<v>";
  Net.iter rule_printer net;
  Format.printf "@]"
  
let rec print_aux ppinfo set = 
  print_rule_net ppinfo set.basic;
  match set.next with
    None -> ()
  | Some set1 -> 
      Format.printf "@,";
      print_aux ppinfo set1

let print ppinfo set = 
  Format.printf "@[<v>[{";
  print_aux ppinfo set;
  Format.printf "}]@]"


(** [make_thm_rule thm]:
   make rule from theorem [thm]
 *)
let make_thm_rule thm=
  make_rule (Logic.RRThm thm) (Logic.term_of thm)

(** [add_simp_rule scp set rls]
   add (properly formed rules) [rls] to set [set]
   in scope [scp]
 *)
let add_simp_rule sset entries=
  List.fold_left (fun s e-> add_rule e s) sset entries

(**
   [thm_to_entries scp thm]: convert a theorem to a list of 
   simpset entries.
 *)   
let thm_to_entries scp thm=
  let rules = Simpconvs.thm_to_rules scp thm
  in 
  List.map make_thm_rule rules

(**
   [simpset_add_thm scp sset thm]: add rewrites from [thm] to
   simpset [sset].
 *)   
let simpset_add_thm scp sset thm=
  let entries = thm_to_entries scp thm
  in 
  add_simp_rule sset entries

let simpset_add_thms scp set thms =
  List.fold_left (simpset_add_thm scp) set thms

let simpset_add_asm sset tg g=
  let trm = 
    Formula.term_of
      (Logic.drop_tag 
	 (Logic.Sequent.get_tagged_asm tg 
	    (Drule.sequent g)))
  in 
  let rule = make_rule (Logic.Asm (Drule.ftag tg)) trm
  in 
  add_simp_rule sset [rule]


(** [make_simp_asms ts except goal]

   Make a list of simp rules from the assumptions with tags 
   in [ts], ignoring those for which [except] is true.
 *)
let make_simp_asm tg goal=
  let trm = 
    Formula.term_of
      (Logic.drop_tag 
	 (Logic.Sequent.get_tagged_asm tg 
	    (Drule.sequent goal)))
  in 
  let rule = make_rule (Logic.Asm (Drule.ftag tg)) trm
  in 
  (tg, rule)

let make_simp_asms tags except goal = 
  let rec make_aux xs rslt=
    match xs with
      [] -> List.rev rslt
    | t::ts -> 
	if(except t) 
	then make_aux ts rslt
	else make_aux ts ((make_simp_asm t goal)::rslt)
  in 
  make_aux tags []

(** [make_simp_asm_rules ts except goal]

   Make a list of simp rules from the tagged formulas
   in [ts], ignoring those for which [except] is true.
 *)
let make_simp_asm_rule tg form=
  let trm =  Formula.term_of form
  in 
  make_rule (Logic.Asm (Drule.ftag tg)) trm

let make_simp_asm_rules except forms = 
  let rec make_aux xs rslt=
    match xs with
      [] -> List.rev rslt
    | (t, f)::fs -> 
	if(except (t, f)) 
	then make_aux fs rslt
	else make_aux fs ((make_simp_asm_rule t f)::rslt)
  in 
  make_aux forms []


