
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
   [simpset]:
   set of simplifier rules 
 *)
type simpset = 
    { 
      basic: rule Termnet.net;          (* global rules *)
      next: simpset option             (* next simpset *)
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
  | Logic.RRThm _ -> Format.print_string "(theorem) ");
  Display.print_term trm;
  Format.close_box();
  Format.print_newline()

let add_rule rl s=
  let (vs, _, l, r, _)=rl
  in 
  if(Simputils.equal_upto_vars (Rewrite.is_free_binder vs) l r)
  then 
    (Format.open_box 0;
     Format.print_string "Ignoring looping rule: ";
     Format.close_box();
     print_rule rl;
     s)
  else 
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
