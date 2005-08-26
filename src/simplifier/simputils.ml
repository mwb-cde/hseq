(*-----
 Name: simputils.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

  (** Utility functions for simplifier *)

open Term
open Logicterm

let dest_rrthm t = 
  match t with 
    Logic.RRThm (x) -> x
  | _ -> failwith("dest_rrth: failure")

let dest_option x=
  match x with
    None -> failwith "dest_option"
  | Some c -> c

let has_cond c =
  match c with
    None -> false
  | Some(_) -> true


(** [dest_implies trm]
   Destruct implication.
   fails if trm is not an implication.
 *)
let dest_implies trm =
  let (f, args)=Term.dest_fun trm
  in
  if(f=Logicterm.impliesid)
  then 
    Lib.get_two args (Failure "dest_implies: too many arguments")
  else raise (Failure "dest_implies: not an implication")

(** [sqnt_solved st b]: 
   true if sqnt st is no longer in branch b 
 *)
let sqnt_solved st b =
  try
    ignore(List.find 
	     (fun x->Tag.equal st (Tactics.sqnt_tag x)) 
	     (Tactics.branch_subgoals b));
    false
  with Not_found ->true

(** [apply_on_cond c t f x]
   if c is None then return [(t x)] else return [(f x)]
 *)
let apply_on_cond c uncondf condf x =
  match c with
    None -> uncondf x
  | Some(_) -> condf x

(** [apply_tag tac n]
   apply tactic [tac] to node [n]
   return new goal and tag record of tactic
 *)
let apply_tag tac n =
  let inf=Tactics.mk_info()
  in 
  let ng = tac inf n
  in 
  (!inf, ng)


(** [apply_get_formula_tag n tac g]
   apply tactic [tac] to goal [g]
   return tags of formulas
   fail if more than [n] new formulas (tags) are generated
 *)
(*
let apply_get_formula_tag n tac g =
  let inf= Tactics.mk_info()
  in 
  let ng = tac inf g
  in 
  let ntg = (!inf).Logic.forms
  in 
  if(List.length ntg)>n 
  then
    raise (Failure "too many tags")
  else (ntg, ng)
*)
let apply_get_formula_tag n tac g =
  let inf= Tactics.mk_info()
  in 
  let ng = tac inf g
  in 
  let atgs = Tactics.aformulas inf
  and ctgs = Tactics.cformulas inf
  in 
  if((List.length atgs) + (List.length ctgs)) >n 
  then
    raise (Failure "too many tags")
  else (atgs, ctgs, ng)

(** [apply_get_single_formula_tag tac g]
   apply tactic [tac] to goal [g]
   return tag of single formula.
   fail if more than 1 new formula is reported by [tac]
 *)
(*
let apply_get_single_formula_tag tac g =
  let ts, ng=apply_get_formula_tag 1 tac g
  in 
  (Lib.get_one ts (Failure "too many tags"), ng)
*)

(** [rebuild_qnt k qs b]
   rebuild quantified term of kind k from quantifiers [qs] and body [b]

   e.g. [rebuild_qnt All ["x", "y", "z"] << b >>]
   ->
   [ << !x y z : b >> ]

   use Drule.rebuild_qnt

 *)
(*
   let rec rebuild_qnt k qs b=
   match qs with
   [] -> b
   | (x::xs) -> Basic.Qnt(k, x, rebuild_qnt k xs b)
 *)


(** [allA_list i vs g]
   apply [allA] to formula [i] using terms [vs]

   use Boolib.inst_asm 
   or Drule.inst_list (fun x -> Logic.Rules.allA None i) vs
 *)
let allA_list l vs = Tactics.instA ~a:l vs
(*
  Drule.inst_list 
    (fun t -> Logic.Tactics.allA None t)
    vs l
*)

(* [make_consts qs env]: 
   Get values for each binder in [qs] from [env].
   Use [base.some] if no value is found.
   [base.some] constant is defined in theory [base].
 *)
(* moved to drule.ml
   let make_consts qs env = 
   let make_aux q=
   try Term.find (Basic.Bound q) env
   with 
   Not_found -> Logicterm.mk_some
   in 
   List.map make_aux qs
 *)

(** [is_variable qnts x]:
   test for variables (universal quantifiers) in an entry 
 *)
let is_variable qnts x= Rewrite.is_free_binder qnts x

(**
 [equal_upto_vars varp x y]: Terms [x] and [y] are equal upto the
   position of the terms for which [varp] is true (which are
   considered to be variables.)

   This is used to determine whether a rewrite- or simp-rule could
   lead to an infinite loop (e.g. |- (x and y) = (y and x) ).
*)
let rec equal_upto_vars varp x y =
  if ((varp x) & (varp y))
  then true
  else 
    match (x, y) with
      (Basic.App(f1, arg1), Basic.App(f2, arg2))->
	(equal_upto_vars varp f1 f2) && (equal_upto_vars varp arg1 arg2)
    | (Basic.Qnt(qn1, b1), Basic.Qnt(qn2, b2)) -> 
	(qn1==qn2) && (equal_upto_vars varp b1 b2)
    | (Basic.Typed(t1, ty1), Basic.Typed(t2, ty2)) ->
	(Gtypes.equals ty1 ty2) && (equal_upto_vars varp t1 t2)
    | (_, _) -> Term.equals x y
