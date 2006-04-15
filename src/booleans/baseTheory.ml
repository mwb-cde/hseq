(*-----
  Name: baseTheory.ml
  Author: M Wahab <mwahab@users.sourceforge.net>
  Copyright M Wahab 2006
  ----*)

open Commands
open Tactics
open Lib.Ops

(**********
* A minimal base theory 
**********)

let builder ()= 
  begin_theory Lterm.base_thy [];
  
  (** Types *)

  let _ = typedef <:def<: ('a, 'b)FUN >> ~pp:(100, infixr, Some("->")) in
  let _ = typedef <:def<: bool >> in 
  let _ = typedef <:def<: ind >> in

  (** Terms *)

  let _ = 
    let prec = BoolPP.negation_pprec.Printer.prec
    and fixity = BoolPP.negation_pprec.Printer.fixity
    in 
      declare
	(Commands.read_unchecked 
	   ((Ident.name_of Lterm.notid)^": bool -> bool"))
	~pp:(prec, fixity, Some "~") in 

  let _ = 
    let prec = BoolPP.negation_pprec.Printer.prec
    and fixity = BoolPP.negation_pprec.Printer.fixity
    in 
      add_term_pp Lterm.notid prec fixity (Some "not") in 

  (** Equality *)
  let _ =
    declare
      (Commands.read_unchecked 
	 ((Ident.name_of Lterm.equalsid)^": 'a -> 'a -> bool"))
      ~pp:(200, infixl, (Some "=")) in 

  (** Conjunction *)
  let _ =
    declare
      (Commands.read_unchecked 
	 ((Ident.name_of Lterm.andid)^":bool->bool->bool"))
      ~pp:(185, infixr, Some "and") in  

  let _ = add_term_pp Lterm.andid 185 infixr (Some "&") in 

  (** Disjunction *)
  let _ =
    define
      (Commands.read_defn ((Ident.name_of Lterm.orid)
			   ^" x y = (not ((not x) and (not y)))"))
      ~pp:(190, infixr, Some "or") in 

  let _ = add_term_pp Lterm.orid 190 infixr (Some "|") in 

  (** Implication *)
  let _ = 
    define
      (Commands.read_defn ((Ident.name_of Lterm.impliesid)
			   ^" x y = (not x) or y"))
      ~pp:(195, infixr, Some "=>") in 

  (** Equivalance *)

  let _ = 
    define
      (Commands.read_defn ((Ident.name_of Lterm.iffid)
			   ^" x y = (x => y) and (y => x)"))
      ~pp:(180, infixn, Some "iff") in 


  (** Axioms *)

  (** False definition *)
  let _ = axiom "false_def" << false = (not true)>> in 

  (** Boolean cases *)
  let _ = 
    axiom "bool_cases" <<!x: (x=true) or (x=false)>> in 

  (** Equality *)

  let _ = 
    axiom "eq_refl" <<!x: x=x>> in 

  let _ =
    define 
    <:def< one_one f = !x1 x2: ((f x1) = (f x2)) => (x1=x2)>> in 

  let _ = 
    define <:def< onto f = !y: ?x: y=(f x)>> in 

  let _ = 
    axiom "infinity_ax" <<?(f: ind -> ind): (one_one f) and (onto f)>> in 

  let _ = 
    axiom "extensionality"  <<!f g: (!x: (f x) = (g x)) => (f = g)>> in 

  (** Specification operator (epsilon) *)
  let _ = declare <<epsilon: ('a -> bool) -> 'a>> in 

  let _ = 
    axiom "epsilon_ax" <<!P: (?x: P x) => (P(epsilon P))>> in 

  (** Conditional *)
  let _ = 
    define
    <:def< 
      IF b t f = (epsilon (%z: (b => (z=t)) and ((not b) => (z=f))))
    >> in 

  (** Any value *)
  let _ = define <:def< any = epsilon (%a: true)>> in 

  (** Unique existence *)
  let _ =
    define 
    <:def<
      EXISTS_UNIQUE p = 
    (? x: (p x)) and (! x y : ((p x) and (p y)) => (x = y))
      >> in 

    end_theory()

let init() = Global.Init.set_base_thy_builder builder 

