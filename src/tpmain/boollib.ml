
open Drule
open Commands
open Tactics

(* Support for if-then-else *)

(** Parser-Printer for If-Then-else *)
module BoolPP = 
  struct
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
	

    let ifthenelse_pprec = Printer.mk_record 10 Printer.nonfix None

    let init_ifthenelse_parser() = 
      Parser.add_symbol "if" (Sym(OTHER "IF"));
      Parser.add_symbol "then" (Sym(OTHER "THEN"));
      Parser.add_symbol "else" (Sym(OTHER "ELSE"));
      Parser.add_term_parser Lib.First "IfThenElse" ifthenelse_parser

(* Printer for If-Then-Else *)

    let ifthenelse_printer ppstate prec (f, args)=
      let cprec=(ifthenelse_pprec.Printer.prec)
      in 
      match args with 
	(b::tbr::fbr::rest) -> 
	  Format.open_box 2;
	  Printer.print_bracket prec cprec "(";
	  Format.print_string "if";
	  Format.print_space();
	  Term.print_term ppstate cprec b;
	  Format.print_space();
	  Format.print_string "then";
	  Format.print_space();
	  Term.print_term ppstate cprec tbr;
	  Format.print_space();
	  Format.print_string "else";
	  Format.print_space();
	  Term.print_term ppstate cprec fbr;
	  Printer.print_bracket prec cprec  ")";
	  if(prec<cprec) then Format.print_space() else ();
	  Format.close_box();
	  (match rest with
	    [] -> ()
	  | _ -> 
	      Format.open_box 0;
	      Printer.print_list
		((fun x ->
		  Format.open_box 0;
		  Term.print_term ppstate prec x;
		  Format.close_box ()),
		 (fun () -> Format.print_space()))
		rest;
	      Format.close_box())	    
      | _ -> 
	  Term.simple_print_fn_app ppstate cprec (f, args)

    let init_ifthenelse_printer()=
      Tpenv.add_term_printer ifthenelse_id
      ifthenelse_printer

    let init_ifthenelse()=
      init_ifthenelse_parser();
      init_ifthenelse_printer()
  end    


(***
 *
 *  Tactics
 *
 ***)

let eq_tac g = 
  let a = first_concl Formula.is_equality (Drule.sequent g)
  in 
  let th = 
    try lemma "base.eq_sym"
    with Not_found -> 
      (raise (Result.error "eq_tac: Can't find required lemma base.eq_sym"))
  in 
  seq [cut th; unify_tac ~a:(fnum (-1)) ~c:a] g

let cut_thm str = (cut (lemma str))

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
    (fst (Term.dest_fun (Formula.term_of_form f)) 
       = (Basic.mk_long "base" "iff"))
  with _ -> false

let iffC_rule i goal = 
  let sqnt=Drule.sequent goal
  in 
  let t, f = Logic.Sequent.get_tagged_cncl (Logic.label_to_tag i sqnt) sqnt
  in
  if not (is_iff f) then (raise (Result.error "iffI_rule"))
  else 
    (seq 
       [Tactics.rewrite_tac [lemma "boolean.iff_def"]
	  ~dir:leftright ~f:(ftag t);
	Logic.Rules.conjC None (ftag t);
	Logic.Rules.implC None (ftag t)]) goal

let iffC ?c g = 
  let cf = 
    match c with
      Some x -> x
    | _ -> (first_concl is_iff (Drule.sequent g))
  in 
  iffC_rule cf g

let false_rule0 a sq =
  let  thm = lemma "base.false_def"
  in 
  seq [(Tactics.rewrite_tac [thm] ~f:a); 
	 Logic.Rules.negA None a; 
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
    (Formula.is_neg, Logic.Rules.negA None);  
    (Formula.is_conj, Logic.Rules.conjA None); 
    (Formula.is_exists, Logic.Rules.existA None)]

let conc_elims () =
  [
   (Formula.is_true, Logic.Rules.trueR None);
   (Formula.is_neg, Logic.Rules.negC None); 
   (Formula.is_disj, Logic.Rules.disjC None);
   (Formula.is_implies, Logic.Rules.implC None);
   (Formula.is_all, Logic.Rules.allC None)]

let rec flatten_tac g =
  repeat
    (Tactics.orl 
       [ Drule.foreach_conc (conc_elims()); 
	 Drule.foreach_asm (asm_elims())]) g


let split_asm () = 
  [(Formula.is_disj, Logic.Rules.disjA None);  
   (Formula.is_implies, Logic.Rules.implA None)]

let split_conc () =
  [(Formula.is_conj, Logic.Rules.conjC None); 
   (is_iff, iffC_rule)]

let rec split_tac g=
  ((orl [ Drule.foreach_conc (split_conc()); 
	  Drule.foreach_asm (split_asm()) ])
    ++
    (split_tac || skip)) g;;

let inst_asm_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	  Tactics.foreach (Logic.Rules.allA None x i) sqs
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
	  Tactics.foreach (Logic.Rules.existC None x i) sqs
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
	    raise (Logic.logicError "inst_tac: No instansiatable formula" [])))
  | Some x -> 
      (try inst_asm ~a:x l g
      with Not_found -> inst_concl ~c:x l g)


let cases_tac0 (x:Basic.term) g= 
  let thm = 
    try
      lemma "boolean.cases_thm"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.cases_thm"))
  in 
  Tactics.seq [cut thm; allA x; disjA; negA; (* postpone *)] g

let cases_tac x = cases_tac0 x

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
      lemma "boolean.equals_bool"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.equals_bool"))
  in 
  (Logic.Rules.rewrite None [Logic.RRThm thm] ff g)

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
    Term.strip_qnt Basic.All (Formula.dest_form f)
  in
  if (Logicterm.is_implies t)
  then
    match Term.dest_fun t with
      (_, (a::b::[])) -> (qnts, a, b)
    | _ -> (raise (Result.error "hyp_conc_thm: unusually shaped implication"))
  else (qnts, Term.mk_bool true, t)

let match_mp_rule0 thm i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.dest_thm thm)
  and c = 
    Formula.dest_form (Drule.get_cncl i sq)
  and scp = Drule.scope_of sq
  and tyenv = Drule.typenv_of sq
  in 
  let qenv = Unify.unify ~typenv:tyenv scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = Drule.make_consts qnts qenv
  and info = Drule.mk_info()
  in 
  ((Logic.Rules.cut (Some(info)) thm)
     ++
     (fun g -> 
       let af = ftag(Lib.get_one (Drule.subgoals info) 
		       (Failure "match_mp_rule"))
       in
       seq
	 [inst_tac ~f:af ncnsts; 
	  Tactics.cut thm; 
	  Logic.Rules.implA None af;
(*	  Logic.Rules.postpone;  *)
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
  and c = Formula.dest_form (Drule.get_cncl i sq)
  and scp = Drule.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = make_consts qnts qenv
  and info =Drule.mk_info()
  in 
  (((inst_tac ~f:j ncnsts) ++ Logic.Rules.implA (Some info) j)
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

(**
   [mp_tac ~a ~f]
   Modus ponens.
   if [a] is [l=>r] and [f] is [l],
   then apply reduce [a] to [r].
*)
let mp_tac ?a ?f g=
  let typenv = Drule.typenv_of g
  and sqnt = Drule.sequent g
  in 
  let scp = Logic.Sequent.scope_of sqnt
  in 
  let mpform_tag =  (* find the implication in the assumptions *)
    match a with
      None -> 
	(try
	  Drule.first_asm
	    (fun x -> 
	      (Logicterm.is_implies) (Formula.dest_form x)) sqnt
	with Not_found ->
	  raise 
	    (Logic.logicError ("mp_tac: no implications in assumptions") []))
    | Some x -> x
  in 
  let mpform = Formula.dest_form (Drule.get_asm mpform_tag g)
  in 
  let (l, r) = 
    match Term.dest_fun mpform with
      (_, (x::y::_)) ->  (x, y)
    | _ -> raise (Invalid_argument "mp_tac")
  in 
  let varp x = false
  in 
  let mpasm_tag = (* find the lhs in the assumptions *)
    match f with
      None -> 
	(try
	  (Drule.match_formulas typenv scp varp l (Logic.Sequent.asms sqnt))
	with Not_found -> 
	  raise (Term.termError ("mp_tac: no match") [mpform]))
    | Some x -> x
  in 
  let info=Drule.mk_info()
  in let g1= (Logic.Rules.implA (Some info) mpform_tag) g
  in let lgtag = 
    match Drule.subgoals info with
      [] -> raise (Result.error "mp_tac: error when applying implA")
    | (x::_) -> x
  in 
  (Logic.foreach 
     ((fun n -> Tag.equal (Drule.node_tag n) lgtag)
	--> 
	  (Logic.Rules.basic (Some info) mpasm_tag 
	     (ftag (Lib.get_one (formulas info) 
		      (Failure "mp_tac: basic"))))) g1)


(*
  let g1= 
    iseq 
      ~initial:([], [], [])
      [(fun info (_, fs, _) ->
	Logic.Rules.implA (Some info) mpform_tag);
       (fun info (_, fs, _) ->
	 thenl skip
	   [Logic.Rules.basic (Some info) mpasm_tag 
	      (ftag (Lib.get_one fs (Failure "mp_tac: basic")));
	    skip])]
      g
  in 
  g1
*)
(*
  if(Tag.equal 
       (Logic.Sequent.sqnt_tag (sequent g1)) 
       (Logic.Sequent.sqnt_tag sqnt))
  then g1
  else raise (Logic.logicError "mp_tac: failed" [])
*)


(***
 *
 * Initialisation
 *
 ***)

let init_boollib()= 
  BoolPP.init_ifthenelse()


let _ = Tpenv.add_init init_boollib



