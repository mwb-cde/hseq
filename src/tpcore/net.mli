(* Term nets: ported from hol98 source then modified*)

  type term_label = 
      Var | Bound | App | Exist | All | Lam 
    | Const of Basic.const_ty 
    | Cname of Basic.fnident

  type 'a net = 
      Tip of 'a list
    |  Node of (term_label * 'a net) list

(*
  type 'a net
*)

  val empty : unit -> 'a net
  val enter: (Term.term -> bool) -> (Term.term * 'a)  -> 'a net -> 'a net
  val lookup : Term.term -> 'a net -> 'a list

(* insert data so that it is stored in an order defined by a given predicate *)
  val insert: ('a -> 'a -> bool)
      -> (Term.term -> bool) -> (Term.term * 'a)  -> 'a net -> 'a net
