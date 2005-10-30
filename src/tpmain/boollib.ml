(*-----
   Name: boollib.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)


(*
open Drule
*)
open Commands
open Tactics

(**********
* A minimal base theory 
**********)
module BaseTheory=
  struct

    let builder() =
      begin_theory Logicterm.base_thy [];
      ignore(typedef 
	       <:def<: ('a, 'b)FUN >> ~pp:(1000, infixr, Some("->")));

      ignore
	(declare
	   (read_unchecked ((Basic.name Logicterm.equalsid)
			    ^": 'a -> 'a -> bool"))
	   ~pp:(1000, infixl, Some"="));
      ignore
	(declare
	   (read_unchecked ((Basic.name Logicterm.notid)^": bool -> bool"))
	   ~pp:(110, prefix, Some "not"));
      ignore
	(declare
	   (read_unchecked ((Basic.name Logicterm.andid)^":bool->bool->bool"))
	   ~pp:(105, infixl, Some "and")); 
      ignore
	(define
	   (read_defn ((Basic.name Logicterm.orid)
		       ^" x y = (not ((not x) and (not y)))"))
	   ~pp:(105, infixl, Some "or"));
      ignore
	(define
	   (read_defn ((Basic.name Logicterm.impliesid)
		       ^" x y = (not x) or y"))
	   ~pp:(104, infixl, Some "=>"));
      ignore
	(define
	   (read_defn ((Basic.name Logicterm.iffid)
		       ^" x y = (x => y) and (y => x)"))
	   ~pp:(104, infixl, Some "iff"));
      ignore(axiom "false_def" << false = (not true)>>);
      ignore(axiom "eq_refl" <<!x: x=x>>);
      ignore(axiom "bool_cases" <<!x: (x=true) or (x=false)>>);
      ignore(declare <<epsilon: ('a -> bool) -> 'a>>);
      ignore(axiom "epsilon_ax" <<!P: (?x: P x) => (P(epsilon P))>>);
      ignore(
      define
	<:def< 
      IF b t f = (epsilon (%z: (b => (z=t)) and ((not b) => (z=f))))
	>>);
      ignore(define <:def< any = epsilon (%a: true)>>);
      ignore(end_theory ~save:false ())
	
    let init() = Global.Init.set_base_thy_builder builder
  end

(**********
* Printer-Parser for Boolean functions. 
**********)
module PP = 
  struct

(**
   Printer for negation. Prints [ << base.not x >> ] 
   as [~x] rather than [~ x].
*)
    let negation_pprec = Printer.mk_record 200 Printer.prefix None

    let negation_printer ppstate (fixity, prec) (f, args)=
      let cprec= negation_pprec.Printer.prec
      and fixity = negation_pprec.Printer.fixity
      in 
      match args with 
      (t::rest) -> 
	Format.printf "@[<2>";
	Printer.print_bracket prec cprec "(";
	Format.printf "~";
	Term.print_term ppstate (fixity, cprec) t;
	Printer.print_bracket prec cprec ")";
	Format.printf "@]";
	(match rest with
	  [] -> ()
	| _ -> 
	    Format.printf "@[";
	    Printer.print_list
	      ((fun x ->
		Term.print_term ppstate (fixity, prec) x),
	       (fun () -> Format.printf "@ "))
	      rest;
	    Format.printf "@]")
      | _ -> 
	  Term.simple_print_fn_app ppstate (fixity, cprec) (f, args)

    let init_negation_printer()=
      Global.PP.add_term_printer Logicterm.notid negation_printer

(***
 Support for if-then-else 
***)

(** Parser-Printer for If-Then-else *)
    open Parser.Pkit
    open Parser.Utility
    open Lexer

    let ifthenelse_id= Basic.mk_long Logicterm.base_thy "IF"

    let ifthenelse_parser inf=
      ((seq
	  [?$(Lexer.Sym (Lexer.OTHER "IF"));
	   Parser.Grammars.form inf;
	   ?$(Lexer.Sym(Lexer.OTHER "THEN"));
	   Parser.Grammars.form inf;
	   ?$(Lexer.Sym(Lexer.OTHER "ELSE"));
	   Parser.Grammars.form inf])
	 >>
       (fun l -> 
	 match l with 
	   [_; test; _; tbr; _; fbr] ->
	     Term.mk_fun ifthenelse_id [test; tbr; fbr]
	 | _ -> raise (ParsingError "Error parsing if-then-else")))
	

    let ifthenelse_pprec = Printer.mk_record 50 Printer.nonfix None

    let init_ifthenelse_parser() = 
      Parser.add_symbol "if" (Sym(OTHER "IF"));
      Parser.add_symbol "then" (Sym(OTHER "THEN"));
      Parser.add_symbol "else" (Sym(OTHER "ELSE"));
      Parser.add_term_parser Lib.First "IfThenElse" ifthenelse_parser

(** Printer for if-then-else **)
    let ifthenelse_printer ppstate (fixity, prec) (f, args)=
      let cfixity = Printer.default_term_fixity
      in 
      let cprec=(ifthenelse_pprec.Printer.prec)
      in 
      match args with 
	(b::tbr::fbr::rest) -> 
	  Format.printf "@[<2>";
	  Printer.print_bracket prec cprec "(";
	  Format.printf "if@ ";
	  Term.print_term ppstate (cfixity, cprec) b;
	  Format.printf "@ then@ ";
	  Term.print_term ppstate (cfixity, cprec) tbr;
	  Format.printf "@ else@ ";
	  Term.print_term ppstate (cfixity, cprec) fbr;
	  Printer.print_bracket prec cprec  ")";
	  if(prec<cprec) then Format.printf "@ " else ();
	  Format.printf "@]";
	  (match rest with
	    [] -> ()
	  | _ -> 
	      Format.printf "@[";
	      Printer.print_list
		((fun x ->
		  Term.print_term ppstate (cfixity, prec) x),
		 (fun () -> Format.printf "@ "))
		rest;
	      Format.printf "@]")
      | _ -> 
	  Term.simple_print_fn_app ppstate (cfixity, cprec) (f, args)

    let init_ifthenelse_printer()=
      Global.PP.add_term_printer ifthenelse_id
	ifthenelse_printer

    let init_ifthenelse()=
      init_ifthenelse_parser();
      init_ifthenelse_printer()


(* Support for printing/parsing [epsilon(%x: P)] as [@x: P] *)

    let choice_ident = Basic.mk_long Logicterm.base_thy "epsilon"
    let choice_sym = "@"
    let choice_pp = 
      (Printer.default_term_fixity, Printer.default_term_prec) 

    let choice_parser =
      Parser.Grammars.parse_as_binder choice_ident choice_sym

    let init_choice_parser ()=
      Parser.add_symbol choice_sym (Lexer.Sym(Lexer.OTHER choice_sym));
      Parser.add_term_parser 
	(Lib.After "lambda") "epsilon" choice_parser
	
    let choice_printer= 
      Term.print_as_binder choice_pp choice_ident choice_sym

    let init_choice_printer () =
      let printer = choice_printer
      in 
      Global.PP.add_term_printer choice_ident 
	(fun ppstate ppenv -> printer ppstate ppenv)

    let init_epsilon() = 
      init_choice_parser();
      init_choice_printer()

(* PP Initialising functions *)

    let init_parsers () = 
      init_ifthenelse_parser();
      init_choice_parser()
      
    let init_printers ()=
      init_negation_printer();
      init_ifthenelse_printer();
      init_choice_printer() 

    let init() =
      init_printers();
      init_parsers();

  end    


(****
* Support functions
*****)

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


(*****
* Tactics
*****)

(*
let false_def = ref None

let false_def() = 
  match !false_def with 
    Some(th) -> th
  |  None -> 
      let th = thm "false_def"
      in 
      false_def:=Some(th); th
*)

let make_false_def () = thm "false_def"
let false_def_var = Lib.freeze make_false_def
let false_def () = Lib.thaw false_def_var

let falseA ?info ?a goal =
  let af= first_asm_label a Formula.is_false goal
  in 
  let th=
    try false_def()
    with Not_found -> 
      raise 
	(Result.error 
	   "falseA: Can't find needed theorem false_def: |- false = not true")
  in 
  let info = Tactics.mk_info()
  in 
  seq 
  [Tactics.rewrite_tac ~info:info [th] ~f:af;
   (fun g -> 
     let a1=get_one ~msg:"falseA" (Tactics.aformulas info)
     in 
     Logic.Tactics.negA (Some info) (ftag a1) g);
   (fun g -> 
     let c=get_one ~msg:"falseA" (Tactics.cformulas info)
     in 
     Logic.Tactics.trueC None (ftag c) g)]
    goal

let trivial ?info ?f g =  
  try (Tactics.trueC ?c:f // falseA ?a:f) g
  with _ -> raise (error "trivial")

let eq_tac ?info ?c g = 
  let cf = first_concl_label c Formula.is_equality g
  in 
  let th = 
    try thm (Logicterm.base_thy ^ ".eq_refl")
    with Not_found -> 
      (raise (error ("eq_tac: Can't find required lemma "
		     ^Logicterm.base_thy^".eq_refl")))
  in 
  let info = Tactics.mk_info()
  in 
  seq [Logic.Tactics.cut (Some info) th; 
       (fun g1 -> 
	 let af = get_one ~msg:"eq_tac" (Tactics.aformulas info)
	 in 
	 unify_tac ~a:(ftag af) ~c:cf g1)] g

let unfold ?info ?f str g= 
  match Lib.try_find defn str with
    None -> 
      raise (error ("unfold: Can't find definition of "^str))
  | (Some th) -> rewrite_tac ?info ?f [th] g

let cut_thm ?info ?inst str = (cut ?info ?inst (thm str))

(*** Boolean equivalence ***)

let is_iff f = 
  try 
    (fst (Term.dest_fun (Formula.term_of f)) = Logicterm.iffid)
  with _ -> false

let make_iff_def () = defn (Basic.string_fnid Logicterm.iffid)
let iff_def_var = Lib.freeze make_iff_def
let iff_def () = Lib.thaw iff_def_var


(** 
   [iffA l sq]: Elminate the equivalance at assumptin [l]

   {L
   g:\[(A iff B){_ l}, asms |- concl]
   ---->
   g:[(A => B){_ l1}, (B => A){_ l2}, asms |- concl]; 
   }

   info: [goals = [], aforms=[l1; l2], cforms=[], terms = []]
 *)
let iffA ?info ?a goal = 
  let af = first_asm_label a is_iff goal
  in 
  let sqnt=Tactics.sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_asm (Logic.label_to_tag af sqnt) sqnt
  in 
    if not (is_iff f) 
    then (raise (error "iffA"))
    else 
      seq 
	[
	  Tactics.rewrite_tac [iff_def()] ~f:(ftag t);
	  Logic.Tactics.conjA info (ftag t);
	] goal
    


(** 
   [iffC l sq]: Elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl]
   ---->
   g1:\[asms |- (A => B){_ l}, concl]
   g2:\[asms |- (B => A){_ l}, concl]
   }

   info: [goals = [g1; g2], aforms=[], cforms=[l], terms = []]
**)

let iffC ?info ?c goal = 
  let cf = first_concl_label c is_iff goal
  in 
  let sqnt=sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  if not (is_iff f) 
  then raise (error "iffC")
  else 
    seq 
      [
	rewrite_tac [iff_def()] ~f:(ftag t);
	Logic.Tactics.conjC info (ftag t)
      ] goal




(** 
   [iffE l sq]: Fully elminate the equivalence at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl]
   ---->
   g1:[A{_ l1}, asms |- B{_ l2}, concl]; 
   g2:[B{_ l3}, asms |- A{_ l4}, concl]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
**)
let iffE ?info ?c goal = 
  let cf = first_concl_label c is_iff goal
  in 
  let sqnt=sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag cf sqnt) sqnt
  in
  let add_goals info inf =
    let gls = subgoals inf
    in 
    Logic.add_info info gls [] [] [];
    empty_info inf
  in 
  let add_forms info inf=
    let atgs = List.rev (aformulas inf)
    and ctgs = List.rev (cformulas inf)
    in 
      Logic.add_info info [] atgs ctgs [];
      empty_info inf
  in
  if not (is_iff f) 
  then raise (error "iffE")
  else 
    let inf = mk_info()
    in 
    let tac g =
      (seq 
	 [
	   rewrite_tac [iff_def()] ~f:(ftag t);
	   notify_tac (add_goals info) inf
	     (Logic.Tactics.conjC (Some inf) (ftag t));
	   Logic.Tactics.implC (Some inf) (ftag t)
	 ]) g
    in 
      alt [ notify_tac (add_forms info) inf tac; 
	    fail ~err:(error "iffE") ] goal

(*** 
* Eliminating boolean operators 
***)

(**
   [direct_alt tacs info l]: Directed alt. Like {!Tactics.alt} but
   pass [info] and [l] to each tactic in [tacs].
**)
let direct_alt tacl info l g=
  let rec alt_aux ts =
    match ts with
      [] -> raise (error "direct_alt: no successful tactic")
    | (x::xs) ->
	try (x info l) g
	with _ -> alt_aux xs
  in alt_aux tacl 


(**
   [direct_map_some tac lst l]: Directed map_some. Like
   {!Tactics.map_som} but pass [info] and [l] to [tac]. If [tac] fails
   for [l], then [lst:=l::!lst].
**)
let direct_map_some tac lst l goal =
  let add_lbl x = lst:=x::(!lst)
  in 
  let nofail_tac lbl = (tac lbl // data_tac add_lbl lbl)
  in 
  let rec some_aux ls g =
    match ls with 
      [] -> fail ~err:(error "direct_map_some: no tactic succeeded.") g
    | (x::xs) ->
	try (tac x ++ map_every nofail_tac xs) g
	with _ -> add_lbl x; some_aux xs g
  in 
  some_aux l goal

(** 
   [asm_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to assumption [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. Any new tag which can't be eliminated are stored in
   [?info] (in arbitrary order).
*)
let rec asm_elim_rules_tac ?info rules lbl goal=
  let (arules, _) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (** 
	 Try to elminate the operator.
       **)
      direct_alt arules inf lbl;
      (** 
	 Eliminate new assumptions and conclusions.
       **)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	   (** Eliminate assumptions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	       skip
	     ];
	   (** Eliminate conclusions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	       skip
	     ];
	   (** Save failing labels and any other information **)
	   data_tac (set_info info)
	     (subgoals inf, 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!alst)), 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!clst)), 
	      constants inf)
	 ] g)
    ] goal
and 

(** 
   [concl_elim_rules ?info (arules, crules) f goal]: Apply elimination
   rules to conclusion [f] and to all resulting assumptions and
   conclusions. Assumptions are eliminated with [arules], conclusions
   with [crules]. The tag of any new formula for which the elimination
   rules fails is stored in [?info] (in arbitrary order).
*)
    concl_elim_rules_tac ?info rules lbl goal=
  let (_, crules) = rules
  and inf = mk_info()
  and alst = ref []
  and clst = ref []
  in 
  let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts
  in 
  seq
    [ 
      (** 
	 Try to elminate the operator.
       **)
      direct_alt crules inf lbl;
      (** 
	 Eliminate new assumptions and conclusions.
       **)
      (fun g -> 
	let albls = List.map ftag (aformulas inf)
	and clbls = List.map ftag (cformulas inf)
	and sqnt = sequent g
	in 
	seq
	  [
	   (** Eliminate conclusions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (concl_elim_rules_tac ?info rules) clst clbls;
	       skip
	     ];
	   (** Eliminate assumptions, saving failing labels **)
	   alt
	     [ 
	       direct_map_some (asm_elim_rules_tac ?info rules) alst albls;
	       skip
	     ];
	   (** Save failing labels and any other information **)
	   data_tac (set_info info)
	     (subgoals inf, 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!alst)), 
	      List.map 
		(fun x -> Logic.label_to_tag x sqnt)  (List.rev (!clst)), 
	      constants inf)
	 ] g)
    ] goal


(**
   [elim_rules_tac ?info (arules, crules) albls clbls]: Apply
   elimination rules to all assumptions with a label in [albls] and
   all conclusions with a label in [clbls] and to all resulting
   assumptions and conclusions. The tag of any new formula for which
   the elimination rules fails is stored in [?info] (in arbitrary
   order).
*)
let elim_rules_tac ?info rules albls clbls =
  match albls with 
    [] -> map_some (concl_elim_rules_tac ?info rules) clbls
  | _ ->
      let chng = ref false
      in 
      let tac g =
	seq
	  [
	   alt 
	     [
	      notify_tac (fun _ -> chng:=true) ()
		(map_some (asm_elim_rules_tac ?info rules) albls);
	      skip
	    ];
	   alt 
	     [ 
	       notify_tac (fun _ -> chng:=true) ()
		 (map_some (concl_elim_rules_tac ?info rules) clbls); 
	       skip 
	     ]
	 ] g
      in 
      restrict (fun _ -> !chng) tac

(**
   [apply_elim_tac tac ?info ?f]: Apply elimination tactic [tac] to
   formula [?f]. If [?f] is not given, use all formulas in the
   sequent. The tag of any new formula for which the elimination rules
   fails is stored in [?info] (in arbitrary order).

   [apply_elim_tac] is a wrapper for [elim_rules_tac].
*)
let apply_elim_tac tac ?info ?f goal =
  let sqnt = sequent goal 
  in 
  let alst, clst = 
    match f with 
      None -> 
	(List.map (fun x -> ftag (drop_formula x)) (asms_of sqnt), 
	 List.map (fun x -> ftag (drop_formula x)) (concls_of sqnt))
    | Some(x) ->
	match Lib.try_find (get_asm x) goal with
	  None -> ([], [x])
	| _ -> ([x], [])
  in 
  tac ?info alst clst goal

(***
  Splitting formulas
***)

let split_asm_rules = 
  [
   (fun inf l -> falseA ~info:inf ~a:l); 
   (fun inf -> Logic.Tactics.disjA (Some inf)); 
   (fun inf -> Logic.Tactics.implA (Some inf))
 ]

let split_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC (Some inf)); 
   (fun inf -> Logic.Tactics.conjC (Some inf)); 
(*   (fun inf c -> iffE ~info:inf ~c:c) *)
 ]


let split_asms_tac ?info lst = 
  asm_elim_rules_tac ?info (split_asm_rules, []) lst

let split_concls_tac ?info lst = 
  concl_elim_rules_tac ?info ([], split_concl_rules) lst

let splitter_tac ?info ?f goal =
  let basic_splitter ?info = 
    elim_rules_tac ?info (split_asm_rules, split_concl_rules)
  in 
  apply_elim_tac basic_splitter ?info ?f goal

let split_tac = splitter_tac 

(***
  Flattening formulas.
***)

let flatter_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l);
   (fun inf -> Logic.Tactics.negA (Some inf));
   (fun inf -> Logic.Tactics.conjA (Some inf));
   (fun inf -> Logic.Tactics.existA (Some inf))
 ]

let flatter_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC (Some inf));
   (fun inf -> Logic.Tactics.negC (Some inf));
   (fun inf -> Logic.Tactics.disjC (Some inf));
   (fun inf -> Logic.Tactics.implC (Some inf));
   (fun inf -> Logic.Tactics.allC (Some inf))
 ]

let flatter_asms_tac ?info lst = 
  asm_elim_rules_tac ?info (flatter_asm_rules, []) lst

let flatter_concls_tac ?info lst = 
  concl_elim_rules_tac ?info ([], flatter_concl_rules) lst

let flatter_tac ?info ?f goal =
  let basic_flatter ?info =
    elim_rules_tac ?info (flatter_asm_rules, flatter_concl_rules)
  in 
  apply_elim_tac basic_flatter ?info ?f goal

let flatten_tac ?info ?f g = flatter_tac ?info:info ?f:f g

(***
 Scattering formulas
***)

let scatter_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l); 

   (fun inf -> Logic.Tactics.negA (Some inf));
   (fun inf -> Logic.Tactics.conjA (Some inf));
   (fun inf -> Logic.Tactics.existA (Some inf));

   (fun inf -> Logic.Tactics.disjA (Some inf)); 
   (fun inf -> Logic.Tactics.implA (Some inf))
 ]

let scatter_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC (Some inf));

   (fun inf -> Logic.Tactics.negC (Some inf));
   (fun inf -> Logic.Tactics.disjC (Some inf));
   (fun inf -> Logic.Tactics.implC (Some inf));
   (fun inf -> Logic.Tactics.allC (Some inf));

   (fun inf -> Logic.Tactics.conjC (Some inf)); 
   (fun inf c -> iffE ~info:inf ~c:c)
 ]

let scatter_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac ?info (scatter_asm_rules, scatter_concl_rules)
  in 
  apply_elim_tac tac ?info ?f goal


(***
 Scattering, solving formulas
***)

let blast_asm_rules =
  [
   (fun inf l -> falseA ~info:inf ~a:l); 

   (fun inf -> Logic.Tactics.negA (Some inf));
   (fun inf -> Logic.Tactics.conjA (Some inf));
   (fun inf -> Logic.Tactics.existA (Some inf));

   (fun inf -> Logic.Tactics.disjA (Some inf)); 
   (fun inf -> Logic.Tactics.implA (Some inf));

   (fun inf l -> basic ~info:inf ~a:l ?c:None)
 ]

let blast_concl_rules =
  [
   (fun inf -> Logic.Tactics.trueC (Some inf));

   (fun inf -> Logic.Tactics.negC (Some inf));
   (fun inf -> Logic.Tactics.disjC (Some inf));
   (fun inf -> Logic.Tactics.implC (Some inf));
   (fun inf -> Logic.Tactics.allC (Some inf));

   (fun inf -> Logic.Tactics.conjC (Some inf)); 
   (fun inf c -> iffE ~info:inf ~c:c);

   (fun inf l -> basic ~info:inf ?a:None ~c:l)
 ]

let blast_tac ?info ?f goal =
  let tac ?info =
    elim_rules_tac ?info (blast_asm_rules, blast_concl_rules)
  in 
  apply_elim_tac tac ?info ?f goal



(*
let inst_asm_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	  Tactics.foreach (Logic.Tactics.allA None x i) sqs
	in rule xs nsqnt
  in rule l (skip sqnt)

let inst_asm ?a l g=
  let af = first_asm_label a Formula.is_all g
  in 
  inst_asm_rule af l g

let inst_concl_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	  Tactics.foreach (Logic.Tactics.existC None x i) sqs
	in rule xs nsqnt
  in rule l (skip sqnt)

let inst_concl ?c l g=
  let cf = first_concl_label c Formula.is_exists g
  in 
  inst_concl_rule cf l g

let inst_tac ?f l g= 
  let sqnt = Tactics.sequent g
  in 
  try inst_asm ?a:f l g
  with _ -> inst_concl ?c:f l g
*)

(***
* Cases
***)

(**
   [cases_tac x sq]

   Adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x.

   {L
   g:\[asms |- concls\]

   ---> 

   g1:\[asms |- x{_ l}, concls\]; g2:\[x{_ l}, asms |- concls\]
   }

   info: [goals = [g1; g2], aforms=[l], cforms=[l], terms = []]
 *)
let make_cases_tac_thm ()= 
    (Commands.get_or_prove "boolean.cases_thm"
       <<!P: (not P) or P>>
     (allC ++ disjC ++ negC ++ basic))

let cases_thm_var = Lib.freeze make_cases_tac_thm
let cases_thm () =  Lib.thaw cases_thm_var

let set_info dst (sgs, afs, cfs, cnsts) = 
    Logic.add_info dst sgs afs cfs cnsts

let cases_tac ?info (t:Basic.term)= 
  let thm = cases_thm()
  and inf1=Tactics.mk_info()
  in 
  seq 
    [
     cut ~info:inf1 thm;
     (fun g -> 
       let thm_tag = get_one ~msg:"cases_tac 1" (aformulas inf1)
       in 
       empty_info inf1; 
       allA ~info:inf1 t ~a:(ftag thm_tag) g);
     (fun g -> 
       let thm_tag = get_one ~msg:"cases_tac 2" (aformulas inf1)
       in 
       empty_info inf1; 
       disjA ~info:inf1 ~a:(ftag thm_tag) g)
       --
       [
	(fun g ->
	  let asm_tag = get_one ~msg:"cases_tac 3" (aformulas inf1)
	  and lgoal, rgoal = get_two ~msg:"cases_tac 4" (subgoals inf1)
	  in 
	  empty_info inf1;
	  seq
	    [
	     negA ~info:inf1 ~a:(ftag asm_tag);
	     (fun g1 -> 
	       let nasm_tag = get_one ~msg:"cases_tac 5" (cformulas inf1)
	       in 
	       data_tac (set_info info)
	       ([lgoal; rgoal], [nasm_tag], [nasm_tag], []) g1);
	   ] g);
	skip
      ]
   ]

let show_tac (trm:Basic.term) tac= 
  let thm = cases_thm()
  and inf1=Tactics.mk_info()
  in 
  seq 
    [
     cut ~info:inf1 thm;
     (fun g -> 
       let thm_tag = get_one ~msg:"show_tac 1" (aformulas inf1)
       in 
       empty_info inf1; 
       allA ~info:inf1 trm ~a:(ftag thm_tag) g);
     (fun g -> 
       let thm_tag = get_one ~msg:"show_tac 2" (aformulas inf1)
       in 
       empty_info inf1; 
       disjA ~info:inf1 ~a:(ftag thm_tag) g)
       --
       [
	(fun g ->
	  let asm_tag = get_one ~msg:"show_tac 3" (aformulas inf1)
	  in 
	  seq [ negA ~a:(ftag asm_tag); tac ] g);
	skip
      ]
   ]

let show = show_tac

(***
* Modus Ponens
***)

let mp_tac ?info ?a ?a1 g=
  let typenv = Tactics.typenv_of g
  and sqnt = Tactics.sequent g
  in 
  let scp = Logic.Sequent.scope_of sqnt
  and asms = asms_of sqnt
  in 
  let (a_label, mp_vars, mp_form) = 
    match a with
      None -> 
	(try (find_qnt_opt Basic.All Logicterm.is_implies asms)
	with 
	  Not_found -> 
	    raise (error "mp_tac: No implications in assumptions") )
    | Some x -> 
	find_qnt_opt Basic.All Logicterm.is_implies [get_tagged_asm x g] 
  and a1_forms = 
    match a1 with
      None -> asms
    | Some(x) -> 
	try [get_tagged_asm x g]
	with Not_found -> 
	  raise (error "mp_tac: No such assumption") 
  in
  let (_, mp_lhs, mp_rhs) = Term.dest_binop mp_form
  in 
  let varp = Rewrite.is_free_binder mp_vars
  in 
  let (a1_label, a1_env)= 
    let exclude (t, _) = (Tag.equal t a_label)
    in
    try 
      (find_unifier scp typenv varp mp_lhs 
	 ~exclude:exclude a1_forms)
    with 
      Not_found -> 
	raise 
	  (Term.term_error ("mp_tac: no matching formula in assumptions") 
	     [Term.mk_fun Logicterm.impliesid [mp_lhs; mp_rhs]])
  in 
  let inf1= Tactics.mk_info()
  in 
  let tac1=
    match mp_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	instA ~a:(ftag a_label)
	  (Tactics.extract_consts mp_vars a1_env)
  and tac2 g2= Logic.Tactics.implA (Some inf1) (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 0 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals inf1) false))
       --> 
	 Logic.Tactics.basic (Some inf1) (ftag a1_label)
	   (ftag (Lib.get_one (Tactics.cformulas inf1) 
		    (Failure "mp_tac2.2")))) g3
  and tac4 g4 = 
    data_tac (set_info info) ([], aformulas inf1, [], []) g4
  in 
  (tac1++ (tac2 ++ tac3 ++ tac4)) g 

(**
   [cut_mp_tac ?info thm ?a]

   Apply modus ponens to theorem [thm] and assumption [a].
   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [a] must be [l].

   If [a] is not given, finds a suitable assumption to unify with [l].

   info [] [thm_tag] [] []
   where tag [thm_tag] identifies the theorem in the sequent.
 *)
let cut_mp_tac ?info ?inst thm ?a g=
  let info1 = Tactics.mk_info()
  and f_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      a None
  in 
  let tac1 = Tactics.cut ~info:info1 ?inst:inst thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Tactics.aformulas info1) 
	(Logic.logic_error "cut_mp_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    mp_tac ?info:info ~a:(ftag a_tag) ?a1:f_label g2)
  in 
  (tac1++tac2) g
    

(**
   Backward match tactic.

   info [g_tag] [] [c_tag] []
   where 
   [g_tag] is the new goal
   [c_tag] identifies the new conclusion.
*)

let back_tac ?info ?a ?c goal=
  let typenv = Tactics.typenv_of goal
  and sqnt = Tactics.sequent goal
  in 
  let scp = Logic.Sequent.scope_of sqnt
  and asms = asms_of sqnt
  and concls = concls_of sqnt
  in 
  let (a_label, back_vars, back_form) = 
    match a with
      None -> 
	(try (find_qnt_opt Basic.All Logicterm.is_implies asms)
	with 
	  Not_found -> 
	    raise (error "back_tac: No implications in assumptions") )
    | Some x -> 
	find_qnt_opt Basic.All Logicterm.is_implies [get_tagged_asm x goal] 
  and c_forms = 
    match c with
      None -> concls
    | Some(x) -> 
	try [get_tagged_concl x goal]
	with Not_found -> 
	  raise (error "back_tac: No such conclusion") 
  in
  let (_, back_lhs, back_rhs) = Term.dest_binop back_form
  in 
  let varp = Rewrite.is_free_binder back_vars
  in 
  (* find, get the conclusion and substitution *)
  let (c_label, c_env)= 
    let exclude (t, _) = (Tag.equal t a_label)
    in 
    try 
      find_unifier scp typenv varp back_rhs 
	~exclude:exclude c_forms
    with 
      Not_found -> 
	raise (Term.term_error 
		 ("back_tac: no matching formula in conclusion") 
		 [Term.mk_fun Logicterm.impliesid [back_lhs; back_rhs]])
  in 
  let info1= Tactics.mk_info()
  in 
  let tac1=
    match back_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	instA ~a:(ftag a_label)
	  (Tactics.extract_consts back_vars c_env)
  and tac2 g2= Logic.Tactics.implA (Some info1) (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 1 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals info1) false))
       --> 
	 Logic.Tactics.basic (Some info1) 
	   (ftag (Lib.get_nth (Tactics.aformulas info1) 1))
	   (ftag c_label)) g3
  in 
  let tac4 g4 = 
    data_tac (set_info info)
     ([Lib.get_nth (Tactics.subgoals info1) 0],
      [], 
      [Lib.get_nth (Tactics.cformulas info1) 0],
      []) g4
  in 
  (tac1++ (tac2 ++ tac3 ++ tac4)) goal


let cut_back_tac ?info ?inst thm ?c g=
  let info1 = Tactics.mk_info()
  and c_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      c None
  in 
  let tac1 = Tactics.cut ~info:info1 ?inst:inst thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Tactics.aformulas info1) 
	(Logic.logic_error "cut_back_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    back_tac ?info:info ~a:(ftag a_tag) ?c:c_label) g2
  in 
  (tac1++tac2) g


module Thms =
  struct
(** 
   {5 Theorems}

   Theorems about boolean operators which may be needed by tactics.
*)


(**
   [make_n_thm()]: prove theorem n
   [n_thm()]: get theorem n, proving it if necessary
*)

(**
   [iff_equals_thm]:  |- !x y: (x iff y) = (x = y)
*)
    let make_iff_equals_thm ()=
      let iff_l2 = 
	let info = Tactics.mk_info()
	in 
	Commands.prove
	  <<!x y: ((x => y) and (y => x)) => (x=y)>>
	(allC ~info:info 
	   ++ allC ~info:info 
	   ++ 
	   (fun g -> 
	     let y_term, x_term = 
	       Lib.get_two (Tactics.constants info) 
		 (Failure "make_iff_equals_thm")
	     in 
	     (flatten_tac
	       ++ (cut_thm "bool_cases" ++ allA x_term)
	       ++ (cut_thm "bool_cases" ++ allA y_term)
	       ++ split_tac 
	       ++ 
	       alt 
	       [(replace_tac ++ (basic // trivial));
		(basic // trivial);
		(replace_tac ++ eq_tac)]) g))
      in 
      let info = Tactics.mk_info()
      in 
      Commands.prove <<!x y: (x iff y) = (x = y)>>
      (allC ~info ++ allC ~info
	 ++ 
	 (fun g -> 
	   let y_term, x_term = 
	     Lib.get_two (Tactics.constants info) 
	       (Failure "make_iff_equals_thm")
	   in 
	   ((cut iff_l2)
	     ++ inst_tac [Logicterm.mk_iff x_term y_term;
			  Logicterm.mk_equality x_term y_term]
	     ++ split_tac
	     --
	     [flatten_tac
		++ cut iff_l2 ++ inst_tac [x_term; y_term]
		    ++ unfold "iff" ~f:(!~2)
		    ++ (implA --  [basic; basic]);
	      flatten_tac
		++ replace_tac
		++ unfold "iff" ~f:(!! 1)
		++ split_tac ++ flatten_tac ++ basic;
	      replace_tac ++ eq_tac]) g))

    let iff_equals_thm_var = Lib.freeze make_iff_equals_thm
    let iff_equals_thm() = Lib.thaw iff_equals_thm_var

(**
   [equals_iff_thm]:  |- !x y: (x = y) = (x iff y)
*)
   let make_equals_iff_thm ()=
     get_or_prove "boolean.equals_bool"
	   << !x y: (x = y) = (x iff y) >>
	 (flatten_tac 
	    ++ (rewrite_tac [iff_equals_thm()])
	    ++ eq_tac)

   let equals_iff_thm_var = Lib.freeze make_equals_iff_thm
   let equals_iff_thm() = Lib.thaw equals_iff_thm_var

(**
   [bool_eq_thm]: |- !x y: x = y = ((x => y) and (y=>x))
 *)
    let make_bool_eq_thm ()= 
      prove << !x y: (x=y) = ((x => y) and (y => x)) >>
      (flatten_tac 
	 ++ rewrite_tac [equals_iff_thm()]
	 ++ unfold "iff"
	 ++ (split_tac ++ flatten_tac ++ split_tac ++ flatten_tac ++ basic))

    let bool_eq_thm_var=  Lib.freeze make_bool_eq_thm
    let bool_eq_thm ()=  Lib.thaw bool_eq_thm_var

(**
   [double_not_thm]: |- ! x: x = (not (not x))
 *)
    let make_double_not_thm () = 
      Commands.prove << !x: x=(not (not x)) >> 
      (flatten_tac ++ rewrite_tac [bool_eq_thm()]
	 ++ split_tac ++ flatten_tac ++ basic)

    let double_not_thm_var = Lib.freeze make_double_not_thm
    let double_not_thm () = Lib.thaw double_not_thm_var

(**
   [rule_true_thm]:  |- !x: x = (x=true) 
 *)

    let make_rule_true_thm ()= 
      let rule_true_l1 =  
	Commands.prove <<!x: (x=true) => x>> 
	(flatten_tac ++ replace_tac ++ trivial)
      in
      let rule_true_l2 = 
	let info = Tactics.mk_info()
	in 
	Commands.prove <<!x: x => (x=true)>>
	(allC ~info:info
	   ++ 
	   (fun g -> 
	     let x_term = 
	       Lib.get_one (Tactics.constants info) 
		 (Failure "rule_true_l2")
	     in 
	     (flatten_tac 
	       ++ (cut_thm "bool_cases") 
	       ++ (allA x_term) 
	       ++ disjA
	       -- 
	       [basic;
		rewrite_tac [Commands.thm "false_def"]
		  ++ replace_tac ++ flatten_tac]) g))
      in
      let rule_true_l3 = 
	Commands.prove <<! x: x iff (x=true)>>
	  ((flatten_tac ++ unfold "iff" ~f:(!! 1) ++ conjC)
	     --
	     [cut rule_true_l2 ++ unify_tac ~a:(!~1) ~c:(!! 1); 
	      cut rule_true_l1 ++ unify_tac ~a:(!~1) ~c:(!! 1)])
      in 
      Logic.Tactics.rewrite_rule (Global.scope()) 
	[iff_equals_thm()] rule_true_l3

    let rule_true_thm_var = Lib.freeze make_rule_true_thm
    let rule_true_thm () = Lib.thaw rule_true_thm_var

(**
   rule_false_thm: !x: (not x) = (x=false)
 *)
    let make_rule_false_thm ()= 
      let info = Tactics.mk_info()
      in 
      Commands.prove <<! x : (not x)=(x=false)>>
      (allC ~info:info
	 ++
	 (fun g -> 
	   let x_term = 
	     Lib.get_one (Tactics.constants info)
	       (Failure "make_rule_false_thm")
	   in 
	   ((once_rewrite_tac [equals_iff_thm()]
	      ++ unfold "iff"
	      ++ split_tac ++ flatten_tac)
	     -- 
	     [
	      cut_thm "bool_cases" ++ inst_tac [x_term]
		++
		(split_tac 
		   ++ replace_tac 
		   ++ (trivial // eq_tac));
	      replace_tac ++ trivial]) g))


    let rule_false_thm_var = Lib.freeze make_rule_false_thm
    let rule_false_thm () = Lib.thaw rule_false_thm_var


(**
   eq_sym_thm: !x y: (x = y) = (y = x)
 *)
    let make_eq_sym_thm ()= 
      Commands.prove << !x y: (x = y) = (y = x) >>
      (allC ++ allC 
       ++ (once_rewrite_tac [equals_iff_thm()])
       ++ iffE
       ++ replace_tac ++ eq_tac);;

    let eq_sym_thm_var = Lib.freeze make_eq_sym_thm
    let eq_sym_thm () = Lib.thaw eq_sym_thm_var

  end


(** 
   Functions to construct theorems from other theorems.
 *)
module Rules=
  struct

(** 
   [once_rewrite_rule scp rules thm]: 
   rewrite [thm] with [rules] once.
 *)
    let once_rewrite_rule scp rules thm =
      let ctrl = {Formula.default_rr_control with Rewrite.depth=Some(1)}
      in 
      Logic.Tactics.rewrite_rule ~ctrl:ctrl scp rules thm


(**
   [conjunctL scp thm]
   Get the left hand side of conjunct [thm].
   [conjunctL scp << l and r >> = l]
*)
    let conjunctL scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Logicterm.is_conj trm)
      then raise (error "conjunct1: not a conjunction")
      else 
	let (_, lhs, rhs) = Term.dest_binop trm
	in 
	let info = Tactics.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut (Some info) thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag ltag) l g1)] g
	in 
	Commands.prove ~scp:scp lhs (proof (fnum 1))

	  
(**
   [conjunctR scp thm]
   Get the right hand side of conjunct [thm].
   [conjunctL scp << l and r >> = r]
*)
    let conjunctR scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Logicterm.is_conj trm)
      then raise (error "conjunct1: not a conjunction")
      else 
	let (_, lhs, rhs) = Term.dest_binop trm
	in 
	let info = Tactics.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut (Some info) thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag rtag) l g1)] g
	in 
	Commands.prove ~scp:scp rhs (proof (fnum 1))

(**
   [conjuncts scp thm]
   break theorem [thm] into the list of conjuncts.
   [conjuncts scp << f1 and f2 and .. and fn>> = [f1; f2; ..; fn]]
*)
    let conjuncts scp thm =
      let is_conj_thm thm = 
	Logicterm.is_conj (Logic.term_of thm)
      in 
      let rec conjuncts_aux scp thm result = 
	if not(is_conj_thm thm)
	then thm::result
	else 
	  let lhs = conjunctL scp thm 
	  and rhs = conjunctR scp thm 
	  in 
	  let result1=conjuncts_aux scp rhs result
	  in 
	  conjuncts_aux scp lhs result1
      in 
      conjuncts_aux scp thm []

(** 
   [conv_rule scp conv thm]
   apply conversion [conv] to theorem [thm]
 *)
    let conv_rule scp conv thm =
      let rule = conv scp (Logic.term_of thm)
      in 
      once_rewrite_rule scp [rule] thm

  end

(** 
   Conversions on boolean operators.
*)
module Convs=
  struct

    open Thms

(** 
   [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a 
*)
    let neg_all_conv scp trm=
      if(not (Logicterm.is_neg trm))
      then failwith "neg_all_conv: not a negation"
      else 
	let (_, trmbody) = Term.dest_unop trm
	in 
	let (aqvars, aqbody) = Term.strip_qnt Basic.All trmbody
	in 
	(match aqvars with 
	  [] -> 
	    failwith 
	      "neg_all_conv: body of negation is not universally quantified"
	| _ -> ());
	let eqvars = 
	  List.map 
	    (fun b ->
	      let (_, n, ty) = Basic.dest_binding b
	      in Basic.mk_binding Basic.Ex n ty) 
	    aqvars
	in 
	let eqbody =
	  let nsubst = 
	    List.fold_left2 
	      (fun s l r -> Term.bind l r s)
	      (Term.empty_subst())
	      (List.map Term.mk_bound aqvars)
	      (List.map Term.mk_bound eqvars)
	  in 
	  Term.subst nsubst aqbody
	in 
	let newterm= 
	  Term.rename 
	    (Term.rebuild_qnt eqvars (Logicterm.mk_not eqbody))
	in 
	let goal_term = 
	  Logicterm.mk_equality trm newterm
	in 
	let info = Tactics.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [bool_eq_thm()] ~f:(fnum 1);
	       Logic.Tactics.conjC None (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag = Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_all_conv: 1")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_all_conv: 1")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negA (Some(info)) (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Tactics.cformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.allC 
					 (Some info) (ftag ctag2));
			       (fun g3 -> 
				 instC ~c:(ftag ctag)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negC (Some info) (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Tactics.aformulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 Tactics.empty_info info;
				 Logic.Tactics.basic 
				   None (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag = Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_all_conv: 4")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_all_conv: 4")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negC (Some(info)) (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Tactics.aformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.existA 
					 (Some info) (ftag atag));
			       (fun g3 -> 
				 instA ~a:(ftag atag2)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negA (Some info) (ftag atag);
			       (fun g3 ->
				 let ctag3 = 
				   Lib.get_one (Tactics.cformulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   None (ftag atag2) (ftag ctag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Commands.prove ~scp:scp goal_term proof

(** 
   [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a 
*)
    let neg_exists_conv scp trm=
      if(not (Logicterm.is_neg trm))
      then failwith "neg_exists_conv: not a negation"
      else 
	let (_, trmbody) = Term.dest_unop trm
	in 
	let (eqvars, eqbody) = Term.strip_qnt Basic.Ex trmbody
	in 
	(match eqvars with 
	  [] -> 
	    failwith 
	      "neg_all_conv: body of negation is not universally quantified"
	| _ -> ());
	let aqvars = 
	  List.map 
	    (fun b ->
	      let (_, n, ty) = Basic.dest_binding b
	      in Basic.mk_binding Basic.All n ty) 
	    eqvars
	in 
	let aqbody =
	  let nsubst = 
	    List.fold_left2 
	      (fun s l r -> Term.bind l r s)
	      (Term.empty_subst())
	      (List.map Term.mk_bound eqvars)
	      (List.map Term.mk_bound aqvars)
	  in 
	  Term.subst nsubst eqbody
	in 
	let newterm= 
	  Term.rename 
	    (Term.rebuild_qnt aqvars (Logicterm.mk_not aqbody))
	in 
	let goal_term = 
	  Logicterm.mk_equality trm newterm
	in 
	let info = Tactics.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [bool_eq_thm()] ~f:(fnum 1);
	       Logic.Tactics.conjC None (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag =
			 Lib.get_one (Tactics.aformulas info)
			   (Failure "neg_exists_conv: 1")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_exists_conv: 1")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negA (Some(info)) (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Tactics.cformulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat (Logic.Tactics.allC 
					 (Some info) (ftag ctag));
			       (fun g3 -> 
				 instC ~c:(ftag ctag2)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negC (Some info) (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Tactics.aformulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 Tactics.empty_info info;
				 Logic.Tactics.basic None 
				   (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag = 
			 Lib.get_one (Tactics.aformulas info) 
			   (Failure "neg_exists_conv: 4")
		       and ctag = 
			 Lib.get_one (Tactics.cformulas info) 
			   (Failure "neg_exists_conv: 4")
		       in 
		       Tactics.empty_info info;
		       seq
			 [
			  Logic.Tactics.negC (Some(info)) (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Tactics.aformulas info)
				(Failure "neg_exists_conv: 2")
			    in 
			    Tactics.empty_info info;
			    seq
			      [repeat 
				 (Logic.Tactics.existA 
				    (Some info) (ftag atag2));
			       (fun g3 -> 
				 instA ~a:(ftag atag)
				   (List.rev (Tactics.constants info)) g3);
			       data_tac 
				 (fun () -> Tactics.empty_info info) ();
			       Logic.Tactics.negA (Some info) (ftag atag);
			       (fun g3 ->
				 let ctag3 = 
				   Lib.get_one (Tactics.cformulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   None (ftag atag2) (ftag ctag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Commands.prove ~scp:scp goal_term proof

  end

(***
* More tactics 
***)

let equals_tac ?info ?f g =
  let ff = 
    match f with
      Some x -> x
    | _ -> 
	try first_asm_label None Formula.is_equality g
	with 
	  Not_found ->
	    try first_concl_label None Formula.is_equality g
	    with Not_found ->
	      raise 
		(error "equals_tac: No equality term")
  in 
  let thm = 
    try Thms.equals_iff_thm()
    with Not_found -> 
      (raise (error "Can't find required lemma boolean.equals_bool"))
  in 
  once_rewrite_tac ?info [thm] ~f:ff g


(***
 *
 * Initialisation
 *
 ***)

let init_boollib()= 
  PP.init()

let _ = Global.Init.add_init init_boollib



(****** Retired 

(***
* Miscellaneous unification functions.
***)

(**
   [unify_formula_for_consts scp trm f]

   Unify [trm] with formula [f] returning the list of terms needed to
   make [trm] alpha-equal to [f] by instantiating the topmost
   quantifiers of [trm].

   raise Not_found, if no unifiable formula found.
*)
let unify_formula_for_consts tyenv scp (vars, trm) f=
  let varp=Rewrite.is_free_binder vars
  in 
  let env=Unify.unify ~typenv:tyenv scp varp trm f
  in 
  extract_consts vars env

(**
   [unify_concl_for_consts ?c trm g]

   if [c] is given, unify [trm] with the conclusion labelled [c],
   returning the list of terms needed to make [trm] alpha-equals
   to the conclusion by instantiating the topmost quantifiers of trm.

   [trm] must be universally quantified.
*)
let unify_concl_for_consts qnt ?c trm node=
  let tyenv = typenv_of node
  and scp = scope_of node
  and (vars, body) = Term.strip_qnt qnt trm
  in 
  let (t, f)=
    match c with 
      None ->
	let sqnt = sequent node
	in 
	let unifies x = 
	  Lib.test_app 
	      (Unify.unify ~typenv:tyenv scp 
		 (Rewrite.is_free_binder vars) body) x
	in 
	first_concl 
	  (fun (_, f) -> unifies (Formula.term_of f)) sqnt
    | Some(x) -> 
	get_tagged_concl x node
  in 
  unify_formula_for_consts tyenv scp (vars, body) (Formula.term_of f)


(**
   [unify_asm_for_consts ?a qnt trm g]

   if [a] is given, unify [trm] with the assumption labelled [a],
   returning the list of terms needed to make [trm] alpha-equals
   to the conclusion by instantiating the topmost quantifiers of trm.

   [trm] must be quantified by [qnt].
*)
let unify_asm_for_consts qnt ?a trm n=
  let tyenv = typenv_of n
  and scp = scope_of n
  and (vars, body) = Term.strip_qnt qnt trm
  in 
  let (t, f)=
    match a with 
      None ->
	  let sqnt = sequent n
	  in 
	  let unifies x = 
	   (try
	     ignore
	       (Unify.unify ~typenv:tyenv scp 
		  (Rewrite.is_free_binder vars) body x);true
	   with _ -> false)
	  in 
	  first_asm (fun (_, f) -> unifies (Formula.term_of f)) sqnt
    | Some(x) ->
	let sqnt = sequent n
	in 
	Logic.Sequent.get_tagged_asm
	  (Logic.label_to_tag x sqnt) sqnt
  in 
  unify_formula_for_consts tyenv scp (vars, body) (Formula.term_of f)

****)
