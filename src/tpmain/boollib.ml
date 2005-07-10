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
      ignore(new_axiom "false_def" << false = (not true)>>);
      ignore(new_axiom "eq_refl" <<!x: x=x>>);
      ignore(new_axiom "bool_cases" <<!x: (x=true) or (x=false)>>);
      ignore(declare <<epsilon: ('a -> bool) -> 'a>>);
      ignore(new_axiom "epsilon_ax" <<!P: (?x: P x) => (P(epsilon P))>>);
      ignore(define
	       <:def< IF b t f = (epsilon (%z: (b => (z=t)) and ((not b) => (z=f))))>>);
      ignore(define <:def< any = epsilon (%a: true)>>);
      ignore(end_theory ~save:false ())
	

    let init() = Global.set_base_thy_builder builder

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
  let cf = 
    match c with
      None -> first_concl Formula.is_equality (Drule.sequent g)
    | Some(x) -> x
  in 
  let th = 
    try lemma "base.eq_refl"
    with Not_found -> 
      (raise (Result.error "eq_tac: Can't find required lemma base.eq_refl"))
  in 
  let info = Drule.mk_info()
  in 
  seq [Logic.Tactics.cut (Some info) th; 
       (fun g1 -> 
	 let af = Lib.get_one (Drule.formulas info) (Failure "eq_tac")
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

let iffA_rule i goal = 
  let sqnt=Drule.sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_asm (Logic.label_to_tag i sqnt) sqnt
  in
  let iff_def_id = Basic.string_fnid Logicterm.iffid
  in 
  if not (is_iff f) 
  then (raise (Result.error "iffC_rule"))
  else 
    (seq 
       [Tactics.rewrite_tac [defn iff_def_id] ~f:(ftag t);
	Logic.Tactics.conjA None (ftag t);
	Logic.Tactics.implA None (ftag t)]) goal

let iffA ?a g = 
  let af = 
    match a with
      Some x -> x
    | _ -> (first_asm is_iff (Drule.sequent g))
  in 
  iffA_rule af g


let iffC_rule i goal = 
  let sqnt=Drule.sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag i sqnt) sqnt
  in
  let iff_def_id = Basic.string_fnid Logicterm.iffid
  in 
  if not (is_iff f) 
  then (raise (Result.error "iffC_rule"))
  else 
    (seq 
       [Tactics.rewrite_tac [defn iff_def_id] ~f:(ftag t);
	Logic.Tactics.conjC None (ftag t);
	Logic.Tactics.implC None (ftag t)]) goal


let iffC ?c g = 
  let cf = 
    match c with
      Some x -> x
    | _ -> (first_concl is_iff (Drule.sequent g))
  in 
  iffC_rule cf g


let get_false_def() = Commands.lemma "false_def"

let falseR ?a goal =
  let af=
    match a with 
      Some x -> x
    | _ -> (Drule.first_asm Formula.is_false (Drule.sequent goal))
  in 
  let th=
    try get_false_def()
    with Not_found -> 
      raise 
	(Result.error 
	   "falseR: Can't find needed theorem false_def: |- false = not true")
  in 
  let info = Drule.mk_info()
  in 
  ((Tactics.rewrite_tac [th] ~f:af)
     ++
     (fun g -> 
       Logic.Tactics.negA (Some info) af g)
     ++
     (fun g -> 
       let c=Lib.get_one (Drule.formulas info) (Failure "falseR")
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
  let af =
    match a with
      Some x -> x
    | _ -> Drule.first_asm Formula.is_false (Drule.sequent goal)
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

let rec flatten_tac g =
  repeat
    (Tactics.alt 
       [ Drule.foreach_conc (conc_elims()); 
	 Drule.foreach_asm (asm_elims())]) g


let split_asm () = 
  [(Formula.is_disj, Logic.Tactics.disjA None);  
   (Formula.is_implies, Logic.Tactics.implA None)]

let split_conc () =
  [(Formula.is_conj, Logic.Tactics.conjC None); 
   (is_iff, iffC_rule)]

let rec split_tac g=
  ((alt [ Drule.foreach_conc (split_conc()); 
	  Drule.foreach_asm (split_asm()) ])
     ++
     (split_tac || skip)) g

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
  let af = 
    match a with 
      Some x -> x
    | _ -> (Drule.first_asm (Formula.is_all) (Drule.sequent g))
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
  let cf = 
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl (Formula.is_exists) (Drule.sequent g))
  in 
  inst_concl_rule cf l g

let inst_tac ?f l g= 
  let sqnt = Drule.sequent g
  in 
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
	  raise (Logic.logic_error "inst_tac: No instansiatable formula" [])))
  | Some x -> 
      (try inst_asm ~a:x l g
      with Not_found -> inst_concl ~c:x l g)


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
	  (Goals.prove <<!P: (not P) or P>>
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
  and tinf=Drule.mk_info()
  in 
  let g1=Logic.Tactics.cut (Some tinf) thm g
  in 
  let ttag=Lib.get_one ((!tinf).Logic.forms) (Failure "case_info")
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
  Logic.add_info inf [ng1;ng2] [ttag] [];
  g2

let cases_tac ?info (x:Basic.term) g = cases_full_tac info x g



let false_tac g = false_rule g

let bool_tac g=
  (false_tac || trivial) g

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
    Formula.term_of (Drule.get_cncl i sq)
  and scp = Drule.scope_of sq
  and tyenv = Drule.typenv_of sq
  in 
  let qenv = Unify.unify ~typenv:tyenv scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = Drule.make_consts qnts qenv
  and info = Drule.mk_info()
  in 
  ((Logic.Tactics.cut (Some(info)) thm)
     ++
     (fun g -> 
       let af = ftag(Lib.get_one (Drule.subgoals info) 
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
    hyp_conc_thm (Drule.get_asm j sq)
  and c = Formula.term_of (Drule.get_cncl i sq)
  and scp = Drule.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = make_consts qnts qenv
  and info =Drule.mk_info()
  in 
  (((inst_tac ~f:j ncnsts) ++ Logic.Tactics.implA (Some info) j)
     ++
     (fun g->
       let gtl, gtr=
	 Lib.get_two (Drule.subgoals info) 
	   (Failure "match_mp_sqnt_rule")
       in 
       seq
	 [(* Logic.goal_focus gtr; *)
	  Tactics.unify_tac ~a:j ~c:i;
	  (* Logic.goal_focus gtl*) ] g)) sq

let back_mp_tac ~a ~c g =match_mp_sqnt_rule0 a c g



(* 
   Modus ponens
 *)
let mp_tac ?a ?a1 g=
  let typenv = Drule.typenv_of g
  and sqnt = Drule.sequent g
  in 
  let scp = Logic.Sequent.scope_of sqnt
  and a_tag = 
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) a None
  and a1_tag = 
    Lib.apply_option (fun x -> Some(Logic.label_to_tag x sqnt)) a1 None
  in 
  let (a_label, mp_vars, mp_form) =
    try
      find_qnt_opt Basic.All ?f:a_tag 
	Logicterm.is_implies (Drule.asms_of sqnt)
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
    try 
      unify_sqnt_form typenv scp varp mp_lhs 
	~exclude:exclude
	?f:a1_tag (Drule.asms_of sqnt)
    with 
      Not_found -> 
	raise 
	  (Term.term_error ("mp_tac: no matching formula in assumptions") 
	     [Term.mk_fun Logicterm.impliesid [mp_lhs; mp_rhs]])
  in 
  let info= Drule.mk_info()
  in 
  let tac1=
    match mp_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	inst_asm ~a:(ftag a_label)
	  (Drule.make_consts mp_vars a1_env)
  and tac2 g2= Logic.Tactics.implA (Some info) (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 0 (Tag.equal (Drule.node_tag n)) 
	 (Drule.subgoals info) false))
       --> 
	 Logic.Tactics.basic (Some info) (ftag a1_label)
	   (ftag (Lib.get_one (Drule.formulas info) 
		    (Failure "mp_tac2.2")))) g3
  in 
  (tac1++ (tac2 ++ tac3)) g 


(*
   [cut_mp_tac ?info thm ?a]

   Apply modus ponens to theorem [thm] and assumption [a].
   [thm] must be a (possibly quantified) implication [!x1 .. xn: l=>r]
   and [a] must be [l].

   If [a] is not given, finds a suitable assumption to unify with [l].

   info [] [thm_tag] []
   where tag [thm_tag] identifies the theorem in the sequent.
 *)
let cut_mp_tac ?info thm ?a g=
  let info1 = Drule.mk_info()
  and f_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Drule.sequent g))))
      a None
  in 
  let tac1 = Logic.Tactics.cut (Some info1) thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Drule.formulas info1) 
	(Logic.logic_error "cut_mp_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    mp_tac ~a:(ftag a_tag) ?a1:f_label g2)
  in 
  let g3= (tac1++tac2) g
  in 
  (Logic.add_info info [] (Drule.formulas info1) [];
   g3)
    

(* 
   Backward match tactic.

   info [g_tag] [c_tag] []
   where 
   [g_tag] is the new goal
   [c_tag] identifies the new conclusion.
 *)
let back_tac ?info ?a ?c g=
  let typenv = Drule.typenv_of g
  and sqnt = Drule.sequent g
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
      Drule.find_qnt_opt Basic.All ?f:a_tag 
	Logicterm.is_implies (Drule.asms_of sqnt)
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
      Drule.unify_sqnt_form typenv scp varp back_rhs 
	~exclude:exclude
	?f:c_tag (Drule.concls_of sqnt)
    with 
      Not_found -> 
	raise (Term.term_error 
		 ("back_tac: no matching formula in conclusion") 
		 [Term.mk_fun Logicterm.impliesid [back_lhs; back_rhs]])
  in 
  let info1= Drule.mk_info()
  in 
  let tac1=
    match back_vars with
      [] -> (* No quantifier *)
	skip
    | _ -> (* Implication has quantifier *)
	inst_asm ~a:(ftag a_label)
	  (Drule.make_consts back_vars c_env)
  and tac2 g2= Logic.Tactics.implA (Some info1) (ftag a_label) g2
  and tac3 g3 =
    ((fun n -> 
      (Lib.apply_nth 1 (Tag.equal (Drule.node_tag n)) 
	 (Drule.subgoals info1) false))
       --> 
	 Logic.Tactics.basic (Some info1) 
	   (ftag (Lib.get_nth (Drule.formulas info1) 1))
	   (ftag c_label)) g3
  in 
  let g4= (tac1++ (tac2 ++ tac3)) g 
  in 
  (Logic.add_info info 
     [Lib.get_nth (Drule.subgoals info1) 0]
     [Lib.get_nth (Drule.formulas info1) 0]
     [];
   g4)

(*
   [back_tac ?info thm ?a]
 *)
let cut_back_tac ?info thm ?c g=
  let info1 = Drule.mk_info()
  and c_label = 
    Lib.apply_option 
      (fun x -> Some (ftag (Logic.label_to_tag x (Drule.sequent g))))
      c None
  in 
  let tac1 = Logic.Tactics.cut (Some info1) thm
  in 
  let tac2 g2 = 
    (let a_tag = 
      Lib.get_one (Drule.formulas info1) 
	(Logic.logic_error "cut_back_tac: Failed to cut theorem" 
	   [Logic.formula_of thm])
    in 
    back_tac ?info:info ~a:(ftag a_tag) ?c:c_label) g2
  in 
  (tac1++tac2) g


let cut_inst_tac ?info thm vals goal = 
  let data = Drule.mk_info()
  in 
  let tac1 = (fun g -> Logic.Tactics.cut (Some data) thm g)
  in 
  let tac2 g = 
    let a=Lib.get_one (Drule.formulas data) (Failure "cut_inst_tac")
    in 
    Logic.add_info info [] [a] [];
    inst_asm_rule (Drule.ftag a) vals g
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
	let info = Drule.mk_info()
	in 
	Goals.prove
	  <<!x y: ((x => y) and (y => x)) => (x=y)>>
	(allC ~info:info 
	   ++ allC ~info:info 
	   ++ 
	   (fun g -> 
	     let y_term, x_term = 
	       Lib.get_two (Drule.constants info) 
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
      let info = Drule.mk_info()
      in 
      Goals.prove <<!x y: (x iff y) = (x = y)>>
      (allC ~info ++ allC ~info
	 ++ 
	 (fun g -> 
	   let y_term, x_term = 
	     Lib.get_two (Drule.constants info) 
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
      Goals.prove << !x y: (x = y) = (x iff y) >>
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
      Goals.prove << !x y: (x=y) = ((x => y) and (y => x)) >>
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
      Goals.prove << !x: x=(not (not x)) >> 
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
	Goals.prove <<!x: (x=true) => x>> 
	(flatten_tac ++ replace_tac ++ trivial)
      in
      let rule_true_l2 = 
	let info = Drule.mk_info()
	in 
	Goals.prove <<!x: x => (x=true)>>
	(allC ~info:info
	   ++ 
	   (fun g -> 
	     let x_term = 
	       Lib.get_one (Drule.constants info) 
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
	Goals.prove <<! x: x iff (x=true)>>
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
      let info = Drule.mk_info()
      in 
      Goals.prove <<! x : (not x)=(x=false)>>
      (allC ~info:info
	 ++
	 (fun g -> 
	   let x_term = 
	     Lib.get_one (Drule.constants info)
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
	let info = Drule.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut (Some info) thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Drule.formulas info) 
		     (Result.error "conjunctL")
		 in 
		 ignore(Drule.empty_info info);
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Drule.formulas info) 
		     (Result.error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag ltag) l g1)] g
	in 
	Goals.prove_goal scp lhs (proof (fnum 1))

	  
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
	let info = Drule.mk_info()
	in 
	let proof l g =
	  seq [Logic.Tactics.cut (Some info) thm;
	       (fun g1 -> 
		 let ttag = 
		   Lib.get_one (Drule.formulas info) 
		     (Result.error "conjunctL")
		 in 
		 ignore(Drule.empty_info info);
		 Logic.Tactics.conjA (Some info) (ftag ttag) g1);
	       (fun g1 -> 
		 let (ltag, rtag)=
		   Lib.get_two (Drule.formulas info) 
		     (Result.error "conjunctL")
		 in 
		 Logic.Tactics.basic None (ftag rtag) l g1)] g
	in 
	Goals.prove_goal scp rhs (proof (fnum 1))

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
	let info = Drule.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [get_bool_eq_ax()] ~f:(fnum 1);
	       Logic.Tactics.conjC None (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag, ctag = 
			 Lib.get_two (Drule.formulas info) 
			   (Failure "neg_all_conv: 1")
		       in 
		       ignore(Drule.empty_info info);
		       seq
			 [
			  Logic.Tactics.negA (Some(info)) (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Drule.formulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    ignore(Drule.empty_info info);
			    seq
			      [repeat (Logic.Tactics.allC 
					 (Some info) (ftag ctag2));
			       (fun g3 -> 
				 inst_concl ~c:(ftag ctag)
				   (List.rev (Drule.constants info)) g3);
			       data_tac 
				 (fun () -> ignore(Drule.empty_info info)) ();
			       Logic.Tactics.negC (Some info) (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Drule.formulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 ignore(Drule.empty_info info);
				 Logic.Tactics.basic 
				   None (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag, ctag = 
			 Lib.get_two (Drule.formulas info) 
			   (Failure "neg_all_conv: 4")
		       in 
		       ignore(Drule.empty_info info);
		       seq
			 [
			  Logic.Tactics.negC (Some(info)) (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Drule.formulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    ignore(Drule.empty_info info);
			    seq
			      [repeat (Logic.Tactics.existA 
					 (Some info) (ftag atag));
			       (fun g3 -> 
				 inst_asm ~a:(ftag atag2)
				   (List.rev (Drule.constants info)) g3);
			       data_tac 
				 (fun () -> ignore(Drule.empty_info info)) ();
			       Logic.Tactics.negA (Some info) (ftag atag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Drule.formulas info)
				     (Failure "neg_all_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   None (ftag atag2) (ftag atag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Goals.prove_goal scp goal_term proof

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
	let info = Drule.mk_info()
	in
	let proof g= 
	  seq [once_rewrite_tac [get_bool_eq_ax()] ~f:(fnum 1);
	       Logic.Tactics.conjC None (fnum 1)
		 --
		 [
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag, ctag = 
			 Lib.get_two (Drule.formulas info) 
			   (Failure "neg_exists_conv: 1")
		       in 
		       ignore(Drule.empty_info info);
		       seq
			 [
			  Logic.Tactics.negA (Some(info)) (ftag atag);
			  (fun g2-> 
			    let ctag2 = 
			      Lib.get_one (Drule.formulas info)
				(Failure "neg_all_conv: 2")
			    in 
			    ignore(Drule.empty_info info);
			    seq
			      [repeat (Logic.Tactics.allC 
					 (Some info) (ftag ctag));
			       (fun g3 -> 
				 inst_concl ~c:(ftag ctag2)
				   (List.rev (Drule.constants info)) g3);
			       data_tac 
				 (fun () -> ignore(Drule.empty_info info)) ();
			       Logic.Tactics.negC (Some info) (ftag ctag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Drule.formulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 ignore(Drule.empty_info info);
				 Logic.Tactics.basic None 
				   (ftag atag3) (ftag ctag2) g3)
			     ] g2)] g1)];
		  
		  seq 
		    [Logic.Tactics.implC (Some info) (fnum 1);
		     (fun g1 ->
		       let atag, ctag = 
			 Lib.get_two (Drule.formulas info) 
			   (Failure "neg_exists_conv: 4")
		       in 
		       ignore(Drule.empty_info info);
		       seq
			 [
			  Logic.Tactics.negC (Some(info)) (ftag ctag);
			  (fun g2-> 
			    let atag2 = 
			      Lib.get_one (Drule.formulas info)
				(Failure "neg_exists_conv: 2")
			    in 
			    ignore(Drule.empty_info info);
			    seq
			      [repeat 
				 (Logic.Tactics.existA 
				    (Some info) (ftag atag2));
			       (fun g3 -> 
				 inst_asm ~a:(ftag atag)
				   (List.rev (Drule.constants info)) g3);
			       data_tac 
				 (fun () -> ignore(Drule.empty_info info)) ();
			       Logic.Tactics.negA (Some info) (ftag atag);
			       (fun g3 ->
				 let atag3 = 
				   Lib.get_one (Drule.formulas info)
				     (Failure "neg_exists_conv: 3")
				 in 
				 Logic.Tactics.basic 
				   None (ftag atag2) (ftag atag3) g3)
			     ] g2)] g1)]]
	     ] g
	in 
	Goals.prove_goal scp goal_term proof



  end

(* More tactics *)

let equals_tac ?f g =
  let ff =
    match f with
      Some x -> x
    | _ -> 
	try
	  (Drule.first_asm Formula.is_equality (Drule.sequent g))
	with 
	  Not_found ->
	    try
	      (Drule.first_concl Formula.is_equality (Drule.sequent g))
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

let _ = Global.add_init init_boollib



