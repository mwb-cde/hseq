
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
      basic: rule Net.net;          (* global rules *)
      next: simpset option             (* next simpset *)
    } 

let empty_set() = 
  { basic=Net.empty(); 
    next=None
  }


(** [termnet_lt varp x y]:

   Less than ordering of terms for use with Net.insert. Makes
   variables (for which [varp] is true) larger than any other term.
 *)
(*
let lt var x y =term_gt (rule_rhs y) (rule_rhs x)
*)

let rec lt_var varp x y=
  let (x_is_var, y_is_var) = (varp x, varp y)
  in 
  match x_is_var, y_is_var with
    (false, true) -> true
  | (true, false) -> false
  | _ -> term_lt varp x y
and term_lt varp t1 t2 = 
  let atom_lt (a1, ty1) (a2, ty2) =  a1<a2
  and bound_lt (q1, n1, _) (q2, n2, _) =  n1<n2 & q1<q1
  in 
  match (t1, t2) with
    Typed (trm, _), _ -> lt_var varp trm t2
  | _, Typed (trm, _) -> lt_var varp t1 trm
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
      if lt_var varp f1 f2 then true
      else if lt_var varp f2 f1 then false
      else lt_var varp a1 a2
  | (App _, _) -> true
  | (Qnt _, Const _) -> false
  | (Qnt _, Id _) -> false
  | (Qnt _, Bound _) -> false
  | (Qnt _, Free _) -> false
  | (Qnt _, App _) -> false
  | (Qnt(qnt1, q1, b1), Qnt(qnt2, q2, b2)) ->
      if (lt_var varp b1 b2) 
      then (bound_lt (dest_binding q1) (dest_binding q2))
      else false
and 
 termnet_lt varp x y = lt_var varp (rule_rhs y) (rule_rhs x)


(** [add_rule rl s]:

   Add rule [rl= c=>(l=r)] to set [s]

   If rule could lead to looping ([Simpconvs.equals_upto_vars l
   r] is true) then make rule an ordered rewrite 
   (not implemented, currently [add_rule] just prints a warning and 
   returns the set [s]).
 *)

let print_rule (vars, cond, lhs, rhs, src)=
  let trm = 
    Drule.rebuild_qnt Basic.All vars
      (match cond with
	None -> (Logicterm.mk_equality lhs rhs)
      | Some c -> (Logicterm.mk_implies c (Logicterm.mk_equality lhs rhs)))
  in 
  Format.open_box 0;
  (match src with
    Logic.Asm _ -> Format.print_string "(assumption) "
  | Logic.OAsm _ -> Format.print_string "(ordered assumption) "
  | Logic.RRThm _ -> Format.print_string "(theorem) ";
  | Logic.ORRThm _ -> Format.print_string "(ordered theorem) ");
  Display.print_term trm;
  Format.close_box();
  Format.print_newline()


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
    { 
      basic=Net.insert (termnet_lt varp) varp (s.basic) l rl1;
      next=s.next
    }
  else 
    { 
      basic=Net.insert (termnet_lt varp) varp (s.basic) l rl;
      next=s.next
    }

(** [lookup trm s]:

   find list of possible matches for term [trm] in set [s].
 *)
let rec lookup set trm =
  try
    Net.lookup set.basic trm 
  with Not_found ->
    (match set.next with 
      None -> raise Not_found
    | Some(s) -> lookup s trm)


(** [join s t]
   Join sets s and t together.
   In the set [join s t], the set [s] will be searched before set [t]
 *)
let rec join s1 s2 = 
  match s1.next with 
    None ->
      { basic=s1.basic;
	next=Some(s2)}
  | Some x -> 
      {basic=s1.basic;
       next=Some(join x s2)}

(** [split s]
   Split simpset [s] into two parts.
   fails if [s] is not joined to another set.
 *)
let split s1=
  match s1.next with 
    None ->
      raise (Failure "Simpset.split")
  | Some x -> 
      ({basic=s1.basic;
	next=None}, x)
	


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
