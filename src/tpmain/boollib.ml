
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
	       Term.mkfun (Basic.mklong "base" "if") [test; tbr; fbr]
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
(*
	  Format.open_box 0;
	  Printer.print_bracket prec cprec "(";
	  Format.print_space();
	  Printer.print_list 
	    ((Term.print_term ppstate cprec),
	     Printer.print_space)
	    args;
	  Printer.print_bracket prec cprec ")";
	  Format.close_box()
*)


    let init_ifthenelse_printer()=
      Tpenv.add_term_printer (Basic.mklong "base" "if")
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

let eq_tac0 sqnt = 
  let a = first_concl Formula.is_equals (Logic.get_sqnt sqnt)
  in 
  let th = 
    try lemma "base.eq_sym"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma base.eq_sym"))
  in thenl [cut th; unify_tac ~a:(fnum (-1)) ~c:a] sqnt

let eq_tac sqnt =  eq_tac0 sqnt

let cut_thm str = (cut (lemma str))

let unfold str i sqnt= 
  let j= if i<0 then i-1 else i
  in
  (thenl[cut (defn str); replace_tac [(fnum(-1))] ~f:(fnum j); 
	 delete (fnum (-1))]) sqnt

(* iffI_rule i sq:
   asm |- a iff b, cncl 
   -->
   a, asm |- b, cncl       and     b, asm |- a, cncl
 *)

let is_iff f = 
  try 
    (fst (Term.dest_fun (Formula.term_of_form f)) 
       = (Basic.mklong "base" "iff"))
  with _ -> false

let iffI_rule i goal = 
  let sqnt=Logic.get_sqnt goal
  in 
  let t, f = Logic.get_tagged_cncl (Logic.label_to_tag i sqnt) sqnt
  in
  if not (is_iff f) then (raise (Result.error "iffI_rule"))
  else 
    (thenl 
       [Tactics.rewrite_tac [lemma "boolean.iff_def"]
	  ~dir:leftright ~f:(ftag t);
	Logic.Rules.conjI None (ftag t);
	Logic.Rules.implI None (ftag t)]) goal

let iffI ?c g = 
  let cf = 
    match c with
      Some x -> x
    | _ -> (first_concl is_iff (Logic.get_sqnt g))
  in 
  iffI_rule cf g

let false_rule0 a sq =
  let  thm = lemma "base.false_def"
  in 
  thenl [(Tactics.rewrite_tac [thm] ~f:a); 
	 Logic.Rules.negA None a; 
	 trivial] sq

let false_rule ?a goal =
  let af =
    match a with
      Some x -> x
    | _ -> Drule.first_asm Formula.is_false (Logic.get_sqnt goal)
  in 
  false_rule0 af goal 

let asm_elims () = 
  [ (Formula.is_false, (fun x -> false_rule ~a:x));
    (Formula.is_neg, Logic.Rules.negA None);  
    (Formula.is_conj, Logic.Rules.conjE None); 
    (Formula.is_exists, Logic.Rules.existI None)]

let conc_elims () =
  [
   (Formula.is_true, Logic.Rules.trueR None);
   (Formula.is_neg, Logic.Rules.negC None); 
   (Formula.is_disj, Logic.Rules.disjE None);
   (Formula.is_implies, Logic.Rules.implI None);
   (Formula.is_all, Logic.Rules.allI None)]

let rec flatten_tac g =
  repeat
    (Tactics.orl 
       [ Drule.foreach_conc (conc_elims()); 
	 Drule.foreach_asm (asm_elims())]) g


let split_asm () = 
  [(Formula.is_disj, Logic.Rules.disjI None);  
   (Formula.is_implies, Logic.Rules.implE None)]

let split_conc () =
  [(Formula.is_conj, Logic.Rules.conjI None); 
   (is_iff, iffI_rule)]

let split_tac g=
  repeat
    (apply_list 
       [ Drule.foreach_conc (split_conc()); 
	 Drule.foreach_asm (split_asm())]) g

let inst_asm_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	  (Logic.Rules.allE None x i) sqs
	in rule xs nsqnt
  in rule l sqnt

let inst_asm ?a l g=
  let af = 
    match a with 
      Some x -> x
    | _ -> (Drule.first_asm (Formula.is_all) (Logic.get_sqnt g))
  in 
  inst_asm_rule af l g

let inst_concl_rule i l sqnt=
  let rec rule ys sqs = 
    match ys with 
      [] -> sqs
    | (x::xs) -> 
	let nsqnt=
	  (Logic.Rules.existE None x i) sqs
	in rule xs nsqnt
  in rule l sqnt

let inst_concl ?c l g=
  let cf = 
    match c with 
      Some x -> x
    | _ -> (Drule.first_concl (Formula.is_all) (Logic.get_sqnt g))
  in 
  inst_concl_rule cf l g

let inst_tac f l g= 
  let sqnt = Logic.get_sqnt g
  in 
  try 
    ignore(Logic.get_label_asm f sqnt);
    inst_asm ~a:f l g
  with Not_found -> inst_concl ~c:f l g

let cases_tac0 (x:Basic.term) g= 
  let thm = 
    try
      lemma "boolean.cases_thm"
    with Not_found -> 
      (raise (Result.error "Can't find required lemma boolean.cases_thm"))
  in 
  thenl [cut thm; allE x; disjI; negA; postpone] g

let cases_tac x = cases_tac0 x

let equals_tac ?f g =
  let ff =
    match f with
      Some x -> x
    | _ -> 
	try
	  (Drule.first_asm Formula.is_equals (Logic.get_sqnt g))
	with 
	  Not_found ->
	    try
	      (Drule.first_concl Formula.is_equals (Logic.get_sqnt g))
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
  (Logic.Rules.rewrite_any None [Logic.RRThm thm] ff g)

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
  else (qnts, Term.mkbool true, t)

let match_mp_rule0 thm i sq=
  let (qnts, a, b) = hyp_conc_thm (Logic.dest_thm thm)
  and c = 
    Formula.dest_form (Drule.get_cncl i sq)
  and scp = Drule.scope_of sq
  in 
  let qenv = Unify.unify scp (Rewrite.is_free_binder qnts) b c
  in 
  let ncnsts = Drule.make_consts qnts qenv
  and info = Drule.mk_info()
  in 
  ((Tactics.cut ~info:info thm)
     ++
     (fun g -> 
       let af = ftag(Lib.get_one (Drule.subgoals info) 
		       (Failure "match_mp_rule"))
       in
       thenl
	 [inst_tac af ncnsts; 
	  Tactics.cut thm; 
	  Logic.Rules.implE None af;
	  Logic.Rules.postpone; 
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
  (((inst_tac j ncnsts) ++ Logic.Rules.implE (Some info) j)
     ++
     (fun g->
       let gtl, gtr=
	 Lib.get_two (Drule.subgoals info) 
	   (Failure "match_mp_sqnt_rule")
       in 
       thenl
	 [Logic.goal_focus gtr;
	  Tactics.unify_tac ~a:j ~c:i;
	  Logic.goal_focus gtl] g)) sq

let back_mp_tac ~a ~c g =match_mp_sqnt_rule0 a c g


(***
 *
 * Initialisation
 *
 ***)

let init_boollib()=
  BoolPP.init_ifthenelse()


let _ = Tpenv.add_init init_boollib



