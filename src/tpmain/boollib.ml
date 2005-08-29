(*-----
   Name: boollib.ml
   Author: M Wahab <mwahab@users.sourceforge.net>
   Copyright M Wahab 2005
   ----*)


open Drule
open Commands
open Tactics

module BaseTheory=
  struct

    (* A minimal base theory *)

    let builder() =
      begin_theory "base" [];
      ignore(typedef <:def<: ('a, 'b)FUN >> ~pp:(1000, infixr, Some("->")));

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
      ignore(define
	       <:def< IF b t f = (epsilon (%z: (b => (z=t)) and ((not b) => (z=f))))>>);
      ignore(define <:def< any = epsilon (%a: true)>>);
      ignore(end_theory ~save:false ())
	

    let init() = Global.Init.set_base_thy_builder builder

  end

module PP = 
  struct

(*
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

(* Support for if-then-else *)
(** Parser-Printer for If-Then-else *)

    open Parser.Pkit
    open Parser.Utility
    open Lexer

    let ifthenelse_id= Basic.mk_long "base" "IF"

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

(* Printer for If-Then-Else *)

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

    let choice_ident = Basic.mk_long "base" "epsilon"
    let choice_sym = "@"
    let choice_pp = 
      (Printer.default_term_fixity, Printer.default_term_prec) 

    let epsilon_parser ()=
      Parser.Grammars.parse_as_binder choice_ident choice_sym

    let init_epsilon_parser ()=
      Parser.add_symbol choice_sym (Lexer.Sym(Lexer.OTHER choice_sym));
      Parser.add_term_parser 
	(Lib.After "lambda") "epsilon" (epsilon_parser())
	
    let epsilon_printer ()= 
      Term.print_as_binder choice_pp choice_ident choice_sym

    let init_epsilon_printer () =
      let printer = epsilon_printer()
      in 
      Global.PP.add_term_printer choice_ident 
	(fun ppstate ppenv -> printer ppstate ppenv)

    let init_epsilon() = 
      init_epsilon_parser();
      init_epsilon_printer()

(* PP Initialising functions *)

    let init_parsers () = 
      init_ifthenelse_parser();
      init_epsilon_parser()
      
    let init_printers ()=
      init_negation_printer();
      init_ifthenelse_printer();
      init_epsilon_printer() 

    let init() =
      init_printers();
      init_parsers();

  end    


(***
 *
 *  Tactics
 *
 ***)

let eq_tac ?c g = 
  let cf =  first_concl_label c Formula.is_equality g
  in 
  let th = 
    try lemma "base.eq_refl"
    with Not_found -> 
      (raise (Result.error "eq_tac: Can't find required lemma base.eq_refl"))
  in 
  let info = Tactics.mk_info()
  in 
  seq [Logic.Tactics.cut (Some info) th; 
       (fun g1 -> 
	 let af = Lib.get_one (Tactics.aformulas info) (Failure "eq_tac")
	 in 
	 unify_tac ~a:(ftag af) ~c:cf g1)] g


let unfold ?f str g= 
  let th = 
    try defn str
    with Not_found -> 
      raise (Result.error ("unfold: Can't find definition of "^str))
  in 
  match f with
    None -> rewrite_tac [th] g
  | Some (x) -> rewrite_tac ~f:x [th] g

(* iffI_rule i sq:
   asm |- a iff b, cncl 
   -->
   a, asm |- b, cncl       and     b, asm |- a, cncl
 *)

let is_iff f = 
  try 
    (fst (Term.dest_fun (Formula.term_of f)) = Logicterm.iffid)
  with _ -> false

(** 
   [iffA l sq]: Elminate the implication at assumptin [l]

   {L
   g:\[(A iff B){_ l}, asms |- concl]
   ---->
   g1:[A{_ l1}, asms |- B{_ l2}, concl]; 
   g2:[B{_ l3}, asms |- A{_ l4}, concl]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
 *)
let iffA_rule ?info i goal = 
  let sqnt=Tactics.sequent goal
  and inf = Tactics.mk_info()
  in 
  let t, f = Logic.Sequent.get_tagged_asm (Logic.label_to_tag i sqnt) sqnt
  in
  let iff_def_id = Basic.string_fnid Logicterm.iffid
  in 
  let set_info () = 
    let subgoals = Tactics.subgoals inf
    and aforms = Tactics.aformulas inf
    and cforms = Tactics.cformulas inf
    in 
    Logic.add_info info (List.rev subgoals) 
      (List.rev aforms) (List.rev cforms) []
  in
  let impl_tac g = 
    let (a, b) = 
      Lib.get_two (Tactics.aformulas inf) (Failure "iffA: impl_tac")
    in 
    Tactics.empty_info inf;
    Tactics.map_every 
      (Logic.Tactics.implA (Some inf)) [ftag a; ftag b]  goal
  in 
  if not (is_iff f) 
  then (raise (Result.error "iffC_rule"))
  else 
    seq 
      [Tactics.rewrite_tac [defn iff_def_id] ~f:(ftag t);
       Logic.Tactics.conjA (Some inf) (ftag t);
       impl_tac;
       Tactics.data_tac set_info ()] goal
    

let iffA ?info ?a g = 
  let af = first_asm_label a is_iff g
  in 
  iffA_rule ?info:info af g


(** 
   [iffC l sq]: Elminate the implication at conclusion [l]

   {L
   g:\[asms |- (A iff B){_ l}, concl]
   ---->
   g1:[A{_ l1}, asms |- B{_ l2}, concl]; 
   g2:[B{_ l3}, asms |- A{_ l4}, concl]; 
   }

   info: [goals = [g1; g2], aforms=[l1; l3], cforms=[l2; l4], terms = []]
 *)
let iffC_rule ?info i goal = 
  let sqnt=Tactics.sequent goal
  and inf = Tactics.mk_info()
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag i sqnt) sqnt
  in
  let iff_def_id = Basic.string_fnid Logicterm.iffid
  in 
  let clear_cforms ()= 
    let subgoals = Tactics.subgoals inf
    in 
    Logic.do_info (Some inf) subgoals [] [] []
  in 
  let set_info () = 
    let subgoals = Tactics.subgoals inf
    and aforms = Tactics.aformulas inf
    and cforms = Tactics.cformulas inf
    in 
    Logic.add_info info subgoals 
      (List.rev aforms) (List.rev cforms) []
  in
  if not (is_iff f) 
  then (raise (Result.error "iffC_rule"))
  else 
    (seq 
       [Tactics.rewrite_tac [defn iff_def_id] ~f:(ftag t);
	Logic.Tactics.conjC (Some inf) (ftag t);
	Tactics.data_tac clear_cforms ();
	Logic.Tactics.implC (Some inf) (ftag t);
	Tactics.data_tac set_info ()]) goal


let iffC ?info ?c g = 
  let cf = first_concl_label c is_iff g
  in 
  iffC_rule ?info cf g


let get_false_def() = Commands.lemma "false_def"

let falseR ?a goal =
  let af= first_asm_label a Formula.is_false goal
  in 
  let th=
    try get_false_def()
    with Not_found -> 
      raise 
	(Result.error 
	   "falseR: Can't find needed theorem false_def: |- false = not true")
  in 
  let info = Tactics.mk_info()
  in 
  ((Tactics.rewrite_tac [th] ~f:af)
     ++
     (fun g -> 
       Logic.Tactics.negA (Some info) af g)
     ++
     (fun g -> 
       let c=Lib.get_one (Tactics.cformulas info) (Failure "falseR")
       in 
       Logic.Tactics.trueR None (ftag c) g)) goal

let trivial ?f g =  
  try (Tactics.trueR ?c:f || falseR ?a:f) g
  with _ -> raise (Result.error "trivial")

let false_rule0 a sq =
  let  thm = lemma "base.false_def"
  in 
  seq [(Tactics.rewrite_tac [thm] ~f:a); 
       Logic.Tactics.negA None a; 
       trivial] sq

let false_rule ?a goal =
  let af = first_asm_label a Formula.is_false goal
  in 
  false_rule0 af goal 

let asm_elims () = 
  [ (Formula.is_false, (fun x -> false_rule ~a:x));
    (Formula.is_neg, Logic.Tactics.negA None);  
    (Formula.is_conj, Logic.Tactics.conjA None); 
    (Formula.is_exists, Logic.Tactics.existA None)]

let conc_elims () =
  [
   (Formula.is_true, Logic.Tactics.trueR None);
   (Formula.is_neg, Logic.Tactics.negC None); 
   (Formula.is_disj, Logic.Tactics.disjC None);
   (Formula.is_implies, Logic.Tactics.implC None);
   (Formula.is_all, Logic.Tactics.allC None)]

let flatten_tac0 g =
  repeat
    (Tactics.alt 
       [ Drule.foreach_conc (conc_elims()); 
	 Drule.foreach_asm (asm_elims())]) g

let split_asm () = 
  [(Formula.is_disj, Logic.Tactics.disjA None);  
   (Formula.is_implies, Logic.Tactics.implA None)]

let split_conc () =
  [(Formula.is_conj, Logic.Tactics.conjC None); 
   (is_iff, iffC_rule ?info:None)]

let rec split0_tac ?info ?f g=
  ((alt [ Drule.foreach_conc (split_conc()); 
	  Drule.foreach_asm (split_asm()) ])
     ++
     (split0_tac || skip)) g

let split_asm_rules info l = 
  alt [Logic.Tactics.disjA info l; Logic.Tactics.implA info l]

let split_concl_rules info l =
   alt [Logic.Tactics.conjC info l; iffC_rule ?info:info l]

let rec split_asms_tac ?info lst=
  let inf = mk_info()
  in 
  seq
    [
     map_some (split_asm_rules (Some inf)) lst;
     (fun g -> 
       ((split_asms_tac ~info:inf (List.map ftag (aformulas inf)) 
       || skip))
	 g);
     data_tac 
       (fun _ -> 
	 Logic.add_info info 
	   (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)) ()
   ]
       
let rec split_concls_tac ?info lst=
  let inf = mk_info()
  in 
  seq
    [
     map_some (split_concl_rules (Some inf)) lst;
     (fun g -> 
       ((split_concls_tac ~info:inf (List.map ftag (cformulas inf)) 
       || skip))
	 g);
     data_tac 
       (fun _ -> 
	 Logic.add_info info 
	   (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)) ()
   ]

let rec basic_splitter ?info alst clst =
  let chng = ref false
  and inf = mk_info()
  in
  Logic.add_info (Some inf) [] [] clst [];
  let set_info () = 
    Logic.add_info info 
      (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)
  in 
  let tac g= 
    seq
      [
       notify_tac (fun _ -> chng:=true)
       ((split_asms_tac ~info:inf (List.map ftag alst)) 
      || split_concls_tac ~info:inf (List.map ftag clst));
       (fun g1 -> 
	 ((basic_splitter ~info:inf 
	   (aformulas inf) (cformulas inf)
	 || skip))
	   g1)
     ] g
  in 
  (restrict (fun _ -> !chng) tac
     ++ data_tac set_info ())

let splitter_tac ?info ?f goal =
  let sqnt = sequent goal 
  in 
  let alst, clst = 
    match f with 
      None -> 
	(List.map drop_formula (asms_of sqnt), 
	 List.map drop_formula (concls_of sqnt))
    | Some(x) ->
	let tg = 
	  try (Logic.label_to_tag x sqnt)
	  with err -> 
	    raise (add_error "splitter_tac: No such formula" err)
	in 
	match Lib.try_find (get_asm x) goal with
	  None -> ([], [tg])
	| _ -> ([tg], [])
  in 
  basic_splitter ?info alst clst goal

let split_tac = splitter_tac 

(** Alternative flattening **)
let flatter_asm_rules info l =
    alt [ false_rule ~a:l;
	  Logic.Tactics.negA info l;
	  Logic.Tactics.conjA info l;
	  Logic.Tactics.existA info l]

let flatter_concl_rules info l =
  alt [Logic.Tactics.trueR None l;
       Logic.Tactics.negC info l; 
       Logic.Tactics.disjC info l;
       Logic.Tactics.implC info l;
       Logic.Tactics.allC info l]

let rec flatter_asms_tac ?info lst =
  let inf = mk_info()
  in 
  seq
    [
     map_some (flatter_asm_rules (Some inf)) lst;
     (fun g -> 
       ((flatter_asms_tac ~info:inf 
	   (List.map ftag (aformulas inf)) 
       || skip))
	 g);
     data_tac 
       (fun _ -> 
	 Logic.add_info info 
	   (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)) ()
   ] 
    
let rec flatter_concls_tac ?info lst =
  let inf = mk_info()
  in 
  seq
    [
     map_some (flatter_concl_rules (Some inf)) lst;
     (fun g -> 
       ((flatter_concls_tac ~info:inf 
	   (List.map ftag (cformulas inf)) 
       || skip))
	 g);
     data_tac 
       (fun _ -> 
	 Logic.add_info info 
	   (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)) ()
   ]
      

let rec basic_flatter ?info alst clst =
  let chng = ref false
  and inf = mk_info()
  in
  Logic.add_info (Some inf) [] alst [] [];
  let set_info () = 
    Logic.add_info info 
      (subgoals inf) (aformulas inf) (cformulas inf) (constants inf)
  in 
  let tac g= 
    seq
      [
       notify_tac
	 (fun _ -> chng:=true)
	 (flatter_concls_tac ~info:inf (List.map ftag clst)
	|| flatter_asms_tac ~info:inf (List.map ftag alst));
       (fun g1 -> 
	 ((basic_flatter ~info:inf 
	   (aformulas inf) (cformulas inf)
	 || skip))
	   g1)
     ] g
  in 
  (restrict (fun _ -> !chng) tac
     ++ data_tac set_info ())


let flatter_tac ?info ?f goal =
  let sqnt = sequent goal 
  in 
  let alst, clst = 
    match f with 
      None -> 
	(List.map drop_formula (asms_of sqnt), 
	 List.map drop_formula (concls_of sqnt))
    | Some(x) ->
	let tg = 
	  try (Logic.label_to_tag x sqnt)
	  with err -> 
	    raise (add_error "flatter_tac: No such formula" err)
	in 
	match Lib.try_find (get_asm x) goal with
	  None -> ([], [tg])
	| _ -> ([tg], [])
  in 
  basic_flatter ?info alst clst goal

(*
let flatten_tac ?info ?f g = flatten_tac0 g
*)
let flatten_tac ?info ?f g = flatter_tac ?info:info ?f:f g

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
(*
  match f with 
    None -> 
      (try 
	let ft = (Drule.first_asm (Formula.is_all) sqnt)
	in inst_asm ~a:ft l g
      with Not_found -> 
	(try 
	  let ft= Drule.first_concl (Formula.is_exists) sqnt
	  in inst_concl ~c:ft l g
	with _ -> 
	  raise (Logic.logic_error "inst_tac: No suitable formula" [])))
  | Some x -> 
      (try inst_asm ~a:x l g
      with Not_found -> inst_concl ~c:x l g)
*)

(**
   [cases_tac x sq]

   Adds formula x to assumptions of sq, 
   creates new subgoal in which to prove x.

   g|asm |- cncl      
   --> 
   g|asm |- t:x, cncl, g'| t:x, asm |- cncl 

   info: [g, g'] [t]
 *)
let cases_tac_thm = ref None

let get_cases_thm ()=
  match !cases_tac_thm with
    None ->
      let nthm =
	try 
	  Commands.lemma "boolean.cases_thm"
	with Not_found -> 
	  (Commands.prove <<!P: (not P) or P>>
	   (allC ++ disjC ++ negC ++ basic))
      in 
      cases_tac_thm := Some(nthm);
      nthm
  | Some(t) -> t

let cases_full_tac inf (t:Basic.term) g= 
  let thm = 
    try get_cases_thm()
    with e -> 
      raise 
	(Result.add_error 
	   (Result.error "cases_full_tac: can't build cases theorem") e)
  and tinf=Tactics.mk_info()
  in 
  let g1=Logic.Tactics.cut (Some tinf) thm g
  in 
  let ttag=Lib.get_one (Tactics.aformulas tinf) (Failure "case_info")
  in 
  let g2=
    foreach
      (seq
	 [Logic.Tactics.allA None t (ftag ttag);
	  Logic.Tactics.disjA (Some tinf) (ftag ttag)
	    -- [Logic.Tactics.negA None (ftag ttag); skip]]) g1
  in 
  let ng1, ng2=Lib.get_two (!tinf).Logic.goals (Failure "case_info")
  in 
  Logic.add_info inf [ng1;ng2] [ttag] [ttag] [];
  g2

let cases_tac ?info (x:Basic.term) g = cases_full_tac info x g



let false_tac g = false_rule g

let bool_tac g=
  (false_tac || trivial) g



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


(**
   [filter_by_tag tg l]: Filter list [l] by an optional tag [tg].  If
   [tg=None], return [l].  If [tg=Some(x)], return the list containing
   only the formula in [l] with tag [x], raising [Not_found] if no
   such formula.
*)
let filter_by_tag tg lst = 
  match tg with 
    None -> lst
  | Some(x) -> 
      let is_tg_form f = Tag.equal x (Tactics.drop_formula f)
      in 
      [Lib.first is_tg_form lst]


(*
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

(* match_mp_rule thm i sqnt: 

   where thm is of the form A=>B and concl i of sqnt is C

   remove outermost universal quantifiers of thm into list qnts
   splits A=>B into (A, B)
   unifies B with C
   cuts thm into sqnt
   instantiates quantifiers of thm with terms obtained by unification.
   applies implE to sqnt, getting a list of sqnts
   applies basic to second sqnt in the sqnt list

   fails if any except first two steps fails 
 *)

let hyp_conc_thm f = 
  let (qnts, t)=
    Term.strip_qnt Basic.All (Formula.term_of f)
  in
  if (Logicterm.is_implies t)
  then
    match Term.dest_fun t with
      (_, (a::b::[])) -> (qnts, a, b)
    | _ -> (raise (Result.error "hyp_conc_thm: unusually shaped implication"))
  else (qnts, Logicterm.mk_true, t)

let match_mp_rule0 thm i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.formula_of thm)
  and c = 
    Formula.term_of (Tactics.get_concl i sq)
  and scp = Tactics.scope_of sq
  and tyenv = Tactics.typenv_of sq
  in 
  let qenv = Unify.unify ~typenv:tyenv scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = Tactics.extract_consts qnts qenv
  and info = Tactics.mk_info()
  in 
  ((Logic.Tactics.cut (Some(info)) thm)
     ++
     (fun g -> 
       let af = ftag(Lib.get_one (Tactics.subgoals info) 
		       (Failure "match_mp_rule"))
       in
       seq
	 [inst_tac ~f:af ncnsts; 
	  Tactics.cut thm; 
	  Logic.Tactics.implA None af;
	  Tactics.unify_tac ~a:af ~c:i] g)) sq

let match_mp_tac thm ?c g = 
  let cf=
    match c with
      None ->  (fnum 1)
    | Some x -> x 
  in 
  match_mp_rule0 thm cf g


let match_mp_sqnt_rule0 j i sq=
  let (qnts, a, b) = 
    hyp_conc_thm (Tactics.get_asm j sq)
  and c = Formula.term_of (Tactics.get_concl i sq)
  and scp = Tactics.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = Tactics.extract_consts qnts qenv
  and info =Tactics.mk_info()
  in 
  (((inst_tac ~f:j ncnsts) ++ Logic.Tactics.implA (Some info) j)
     ++
     (fun g->
       let gtl, gtr=
	 Lib.get_two (Tactics.subgoals info) 
	   (Failure "match_mp_sqnt_rule")
       in 
       seq
	 [(* Logic.goal_focus gtr; *)
	  Tactics.unify_tac ~a:j ~c:i;
	  (* Logic.goal_focus gtl*) ] g)) sq

let back_mp_tac ~a ~c g =match_mp_sqnt_rule0 a c g


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


(***  Modus ponens ***)

let mp_tac ?a ?a1 g=
  let typenv = Tactics.typenv_of g
  and sqnt = Tactics.sequent g
  in 
  let scp = Logic.Sequent.scope_of sqnt
  and a_tag = 
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) a None
  and a1_tag = 
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) a1 None
  in 
  let (a_label, mp_vars, mp_form) =
    try
      find_qnt_opt Basic.All 
	Logicterm.is_implies 
	(filter_by_tag a_tag (Tactics.asms_of sqnt))
    with Not_found -> 
      raise (Logic.logic_error ("mp_tac: no implications in assumptions") 
	       [])
  in
  let (_, mp_lhs, mp_rhs) = Term.dest_binop mp_form
  in 
  let varp = Rewrite.is_free_binder mp_vars
  in 
  let (a1_label, a1_env)= 
    let exclude (t, _) = (Tag.equal t a_label)
    in
    (try 
      find_unifier scp typenv varp mp_lhs 
	~exclude:exclude
	(filter_by_tag a1_tag (Tactics.asms_of sqnt))
    with 
      Not_found -> 
	raise 
	  (Term.term_error ("mp_tac: no matching formula in assumptions") 
	     [Term.mk_fun Logicterm.impliesid [mp_lhs; mp_rhs]]))
  in 
  let info= Tactics.mk_info()
  in 
  let tac1=
    match mp_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	inst_asm ~a:(ftag a_label)
	  (Tactics.extract_consts mp_vars a1_env)
  and tac2 g2= Logic.Tactics.implA (Some info) (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 0 (Tag.equal (Tactics.node_tag n)) 
	 (Tactics.subgoals info) false))
       --> 
	 Logic.Tactics.basic (Some info) (ftag a1_label)
	   (ftag (Lib.get_one (Tactics.cformulas info) 
		    (Failure "mp_tac2.2")))) g3
  in 
  (tac1++ (tac2 ++ tac3)) g 


(*
   [cut_mp_tac ?info thm ?a]

   Apply modus ponens to theorem [thm] and assumption [a].
   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [a] must be [l].

   If [a] is not given, finds a suitable assumption to unify with [l].

   info [] [thm_tag] [] []
   where tag [thm_tag] identifies the theorem in the sequent.
 *)
let cut_mp_tac ?info thm ?a g=
  let info1 = Tactics.mk_info()
  and f_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      a None
  in 
  let tac1 = Logic.Tactics.cut (Some info1) thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Tactics.aformulas info1) 
	(Logic.logic_error "cut_mp_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    mp_tac ~a:(ftag a_tag) ?a1:f_label g2)
  in 
  let g3= (tac1++tac2) g
  in 
  (Logic.add_info info [] (Tactics.aformulas info1) [] [];
   g3)
    

(* 
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
  and a_tag =  (* assumption having implication *)
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) a None
  and c_tag =  (* conclusion to match with rhs of assumption *)
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) c None
  in 
  (* find, get the assumption *)
  let (a_label, back_vars, back_form) =
    try 
      find_qnt_opt Basic.All 
	Logicterm.is_implies 
	(filter_by_tag a_tag (Tactics.asms_of sqnt))
    with Not_found -> 
      raise (Logic.logic_error ("back_tac: no implications in assumptions") 
	       [])
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
	~exclude:exclude
	(filter_by_tag c_tag  (Tactics.concls_of sqnt))
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
	inst_asm ~a:(ftag a_label)
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
  let g4= (tac1++ (tac2 ++ tac3)) goal
  in 
  (Logic.add_info info 
     [Lib.get_nth (Tactics.subgoals info1) 0]
     []
     [Lib.get_nth (Tactics.cformulas info1) 0]
     [];
   g4)

(*
   [back_tac ?info thm ?a]
 *)
let cut_back_tac ?info thm ?c g=
  let info1 = Tactics.mk_info()
  and c_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Tactics.sequent g))))
      c None
  in 
  let tac1 = Logic.Tactics.cut (Some info1) thm
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


let cut_inst_tac ?info thm vals goal = 
  let data = Tactics.mk_info()
  in 
  let tac1 = (fun g -> Logic.Tactics.cut (Some data) thm g)
  in 
  let tac2 g = 
    let a=Lib.get_one (Tactics.aformulas data) (Failure "cut_inst_tac")
    in 
    Logic.add_info info [] [a] [] [];
    inst_asm_rule (ftag a) vals g
  in 
  let goal1 = (tac1 ++ tac2) goal
  in 
  goal1


let cut_thm str = (cut (lemma str))

module Props =
  struct
(** 
   Props: Basic boolean properties. 
 *)

(**
   [make_n_ax()]: prove theorem n
   [get_n_ax()]: get theorem n, proving it if necessary

   [iff_equals_ax]:  |- !x y: (x iff y) = (x = y)
 *)
    let make_iff_equals_ax ()=
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
		 (Failure "make_iff_equals_ax")
	     in 
	     (flatten_tac
	       ++ (cut_thm "bool_cases" ++ allA x_term)
	       ++ (cut_thm "bool_cases" ++ allA y_term)
	       ++ split_tac 
	       ++ 
	       alt 
	       [(replace_tac ++ (basic || trivial));
		(basic || trivial);
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
	       (Failure "make_iff_equals_ax")
	   in 
	   (
(* flatten_tac ++ *)
	      (cut iff_l2)
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

    let iff_equals_ax = ref None
    let get_iff_equals_ax ()=
      match !iff_equals_ax with
	None -> 
	  let nthm = make_iff_equals_ax()
	  in 
	  iff_equals_ax := Some(nthm);
	  nthm
      | Some(x) -> x


(**
   [equals_iff_ax]:  |- !x y: (x = y) = (x iff y)
 *)
    let make_equals_iff_ax ()=
      Commands.prove << !x y: (x = y) = (x iff y) >>
      (flatten_tac 
	 ++ (rewrite_tac [get_iff_equals_ax()])
	 ++ eq_tac)

    let equals_iff_ax = ref None
    let get_equals_iff_ax ()=
      match !equals_iff_ax with
	None -> 
	  let nthm = make_equals_iff_ax()
	  in 
	  equals_iff_ax := Some(nthm);
	  nthm
      | Some(x) -> x


(**
   [bool_eq_ax]: |- !x y: x = y = ((x => y) and (y=>x))
 *)
    let make_bool_eq_ax () = 
      Commands.prove << !x y: (x=y) = ((x => y) and (y => x)) >>
      (flatten_tac 
	 ++ rewrite_tac [get_equals_iff_ax()]
	 ++ unfold "iff"
	 ++ (split_tac ++ flatten_tac ++ split_tac ++ flatten_tac ++ basic))

    let bool_eq_ax = ref None
    let get_bool_eq_ax ()=
      match !bool_eq_ax with
	None -> 
	  let nthm = make_bool_eq_ax()
	  in 
	  bool_eq_ax := Some(nthm);
	  nthm
      | Some(x) -> x

(**
   [double_not_ax]: |- ! x: x = (not (not x))
 *)
    let make_double_not_ax () = 
      Commands.prove << !x: x=(not (not x)) >> 
      (flatten_tac ++ rewrite_tac [get_bool_eq_ax()]
	 ++ split_tac ++ flatten_tac ++ basic)

    let double_not_ax = ref None
    let get_double_not_ax ()=
      match !double_not_ax with
	None -> 
	  let nthm = make_double_not_ax()
	  in 
	  double_not_ax := Some(nthm);
	  nthm
      | Some(x) -> x

(**
   [rule_true_ax]:  |- !x: x = (x=true) 
 *)
    let make_rule_true_ax ()= 
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
		rewrite_tac [Commands.lemma "false_def"]
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
	[get_iff_equals_ax()] rule_true_l3

    let rule_true_ax = ref None

    let get_rule_true_ax ()= 
      match !rule_true_ax with
	None -> 
	  let nthm =make_rule_true_ax()
	  in 
	  rule_true_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

(**
   rule_false_ax: !x: (not x) = (x=false)
 *)
    let make_rule_false_ax ()= 
      let info = Tactics.mk_info()
      in 
      Commands.prove <<! x : (not x)=(x=false)>>
      (allC ~info:info
	 ++
	 (fun g -> 
	   let x_term = 
	     Lib.get_one (Tactics.constants info)
	       (Failure "make_rule_false_ax")
	   in 
	   ((
(* flatten_tac ++  *)
	       once_rewrite_tac [get_equals_iff_ax()]
	      ++ unfold "iff"
	      ++ split_tac ++ flatten_tac)
	     -- 
	     [
	      cut_thm "bool_cases" ++ inst_tac [x_term]
		++
		(split_tac 
		   ++ replace_tac 
		   ++ (trivial || eq_tac));
	      replace_tac ++ trivial]) g))

    let rule_false_ax = ref None
    let get_rule_false_ax ()= 
      match !rule_false_ax with
	None -> 
	  let nthm =make_rule_false_ax()
	  in 
	  rule_false_ax:=Some(nthm);
	  nthm
      | Some(t) -> t

  end


module Rules=
  struct
(** 
   Rules: Functions to construct theorems from other theorems.
   These may depend on the theorems in Props.
 *)

(** [once_rewrite_rule scp rules thm]: 
   rewrite [thm] with [rules] once.
 *)
    let once_rewrite_rule scp rules thm =
      let ctrl = {Formula.default_rr_control with Rewrite.depth=Some(1)}
      in 
      Logic.Tactics.rewrite_rule ~ctrl:ctrl scp rules thm


(*
   [conjunctL scp thm]
   Get the left hand side of conjunct [thm].
   [conjunctL scp << l and r >> = l]
 *)
    let conjunctL scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Logicterm.is_conj trm)
      then raise (Result.error "conjunct1: not a conjunction")
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
		     (Result.error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (Result.error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag ltag) l g1)] g
	in 
	Commands.prove ~scp:scp lhs (proof (fnum 1))

	  
(*
   [conjunctR scp thm]
   Get the right hand side of conjunct [thm].
   [conjunctL scp << l and r >> = r]
 *)
    let conjunctR scp thm = 
      let trm = Logic.term_of thm
      in 
      if not (Logicterm.is_conj trm)
      then raise (Result.error "conjunct1: not a conjunction")
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
		     (Result.error "conjunctL")
		 in 
		 Tactics.empty_info info;
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Tactics.aformulas info) 
		     (Result.error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag rtag) l g1)] g
	in 
	Commands.prove ~scp:scp rhs (proof (fnum 1))

(*
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

  end

(** [conv_rule scp conv thm]
   apply conversion [conv] to theorem [thm]
 *)
let conv_rule scp conv thm =
  let rule = conv scp (Logic.term_of thm)
  in 
  Rules.once_rewrite_rule scp [rule] thm

module Convs=
  struct
(** 
   Convs: Conversions on boolean operators.
   These may depend on the theorems in Props.
 *)

    open Props

(** [neg_all_conv]: |- (not (!x..y: a)) = ?x..y: not a *)
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
	  seq [once_rewrite_tac [get_bool_eq_ax()] ~f:(fnum 1);
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
				 inst_concl ~c:(ftag ctag)
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
				 inst_asm ~a:(ftag atag2)
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

(** [neg_exists_conv]: |- (not (?x..y: a)) = !x..y: not a *)
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
	  seq [once_rewrite_tac [get_bool_eq_ax()] ~f:(fnum 1);
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
				 inst_concl ~c:(ftag ctag2)
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
				 inst_asm ~a:(ftag atag)
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

(* More tactics *)

let equals_tac ?f g =
  let ff = 
    match f with
      Some x -> x
    | _ -> 
	try
	  first_asm_label None Formula.is_equality g
	with 
	  Not_found ->
	    try
	      first_concl_label None Formula.is_equality g
	    with Not_found ->
	      raise 
		(Result.error "equals_tac: No equality term")
  in 
  let thm = 
    try
      Props.get_equals_iff_ax()
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.equals_bool"))
  in 
  once_rewrite_tac [thm] ~f:ff g

(*
  (Logic.Tactics.rewrite None [Logic.RRThm thm] ff g)
*)

(***
 *
 * Initialisation
 *
 ***)

let init_boollib()= 
  PP.init()

let _ = Global.Init.add_init init_boollib



