
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


(** 
   [simp_set]:
   set of simplifier rules 
 *)
type simp_set = 
    { 
      basic: rule Termnet.net;          (* global rules *)
      next: simp_set option             (* next simp_set *)
    } 

let empty_set() = 
  { basic=Termnet.empty(); 
    next=None
  }


(** [lt x y]:
   less than ordering of term
 *)
let lt (_, _, _, x, _) (_, _, _, y, _)=term_gt y x

(** [add_rule rl s]:

   add rule [rl] to set [s]
 *)
let add_rule rl s=
  let (vs, _, l, _, _)=rl
  in 
  { 
    basic=Termnet.insert lt (is_variable vs) (s.basic) l rl;
    next=s.next
  }

(** [lookup trm s]:

   find list of possible matches for term [trm] in set [s].
 *)
let rec lookup set trm =
  try
    Termnet.lookup set.basic trm 
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
   Split simp_set [s] into two parts.
   fails if [s] is not joined to another set.
 *)
let split s1=
  match s1.next with 
    None ->
      raise (Failure "Simpset.split")
  | Some x -> 
      ({basic=s1.basic;
	next=None}, x)
	

