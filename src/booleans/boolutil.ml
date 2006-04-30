(*-----
  Name: boolutil.ml
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2006
  ----*)

(***
* Utility functions for boolean reasoning 
***)

open Commands
open Tactics
open Lib.Ops


(**
   [find_unifier scp typenv varp trm ?exclude ?f forms]: Find the first
   formula in forms which unifies with trm. Return the tag of the
   formula and the substitution cosntructed by unification. Ignore
   those formulas for which [?exclude] is true (if it is given).

   [varp] determines what is a bindable variable for unification.
   [typenv] is the type environment, to pass to the unifier.
   [scp] is the scope, to pass to the unifier.
   Raise Not_found if no unifiable formula is found.
*)
let find_unifier scp typenv varp trm ?exclude forms = 
  let not_this = Lib.get_option exclude (fun _ -> false)
  in 
  let find_fn form =
    if (not_this form) then raise Not_found
    else 
      (Tactics.drop_formula form,
       Unify.unify ~typenv:typenv scp varp trm 
	 (Formula.term_of (Tactics.drop_tag form)))
  in 
  Lib.find_first find_fn forms


(**
   [is_qnt_opt kind pred form]: Test whether [form] satifies [pred].
   The formula may by quantified by binders of kind [kind]. 
*)
let is_qnt_opt kind pred form = 
  Tactics.qnt_opt_of kind pred 
    (Formula.term_of (Tactics.drop_tag form))

(**
   [dest_qnt_opt forms]: Destruct a possibly quantified tagged formula.
   Returns the binders, the tag and the formula.
*)
let dest_qnt_opt kind tform = 
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform
  in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in 
  (tag, vs, term)
	
(**
   [find_qnt_opt kind ?f pred forms] 

   Find the first formula in [forms] to satisfy [pred].  The formula
   may by quantified by binders of kind [kind].  Returns the binders,
   the tag and the formula.

   if [f] is given, the formula must be tagged with [f]. 

   Raises [Not_found] if no formula can be found which satisfies all the
   conditions.
 *)
let find_qnt_opt kind pred forms = 
  let find_fn tagged_form =
    Tactics.qnt_opt_of kind pred 
      (Formula.term_of (Tactics.drop_tag tagged_form))
  in 
  let tform = Lib.first find_fn forms
  in 
  let tag = Tactics.drop_formula tform
  and form = Tactics.drop_tag tform
  in
  let (vs, term) = Term.strip_qnt kind (Formula.term_of form)
  in 
  (tag, vs, term)

let fresh_thm th = Logic.is_fresh (Global.scope()) th

let get_type_name ty =
  match ty with
    Basic.Constr (id, _) -> id
  | _ -> failwith "get_type_name"
