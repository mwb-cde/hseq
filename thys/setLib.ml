(*-----
Name: setLib.ml
Author: M Wahab <mwahab@users.sourceforge.net>
Copyright M Wahab 2006
----*)

let set_thy = "Set"
let set_id = Ident.mk_long set_thy "SET"
let set_data = (Printer.default_term_fixity, Printer.default_term_prec)
let set_ty ()= 
  Gtypes.mk_def (Ident.mk_long set_thy "set") [Gtypes.mk_null()]

let single_id = Ident.mk_long set_thy "single"
let single_data = (Printer.default_term_fixity, Printer.default_term_prec)

let empty_id = Ident.mk_long set_thy "empty"
let empty_data = (Printer.default_term_fixity, Printer.default_term_prec)
let empty_term ()= Term.mk_typed_var empty_id (set_ty())

let add_id = Ident.mk_long set_thy "add"

module SetPP =
  struct

    let ocb_sym, ccb_sym = ("{", "}")
    let semicolon_sym = ";" 

    (* Parser *)
    open Parser.Pkit
    open Parser.Grammars
    open Parser.Utility
    open Lexer

    let set_parser () =
      let ocb_tok = Sym(OTHER ocb_sym)
      and ccb_tok = Sym(OTHER ccb_sym)
(*      and semicolon = Sym(OTHER semicolon_sym) *)
      in 
      let wrapper t = 
	let id_term = Term.mk_var set_id
	in 
	Term.mk_app id_term t
      in 
      let colon = Sym(COLON)
      in 
      let set_body inf inp =
	(((((id_type_opt (short_id id) inf)
	      -- (?$ colon))
	     >> 
	   (fun (v, _) -> 
	     Parser.Grammars.qnt_setup_bound_names inf Basic.Lambda [v]))
	    -- (form inf))
	   >>
	 (fun ((xs:(string*Basic.term)list), body) ->
	   make_term_remove_names inf wrapper xs body)) inp
      in 
      let main_parser inf inp = 
	(seq
	   [?$ ocb_tok;
	    (optional 
	       (alt 
		  [ 
		    set_body inf; 
		    ((Parser.Grammars.comma_list (form inf))
		      >> 
		    (fun ts -> 
		      List.fold_left
			(fun st elt -> Term.mk_fun add_id [elt; st])
			(empty_term()) ts))

(*
   ((form inf) >> (fun t -> Term.mk_app (Term.mk_var single_id) t))
*)
		  ]))
	      >>
	    (fun s -> Lib.get_option s (empty_term()));
	    ?$ ccb_tok]
	   >> 
	 (fun l ->
	   match l with
	     [_; s; _] -> s
	   | _ -> raise (ParsingError "Not a set"))) inp
      in 
      main_parser

    let init_set_parser ()= 
      Parser.add_symbol ocb_sym (Lexer.Sym(Lexer.OTHER ocb_sym));
      Parser.add_symbol ccb_sym (Lexer.Sym(Lexer.OTHER ccb_sym));
      Parser.add_term_parser Lib.Last "Set" (set_parser())


	(* Printer *)
    let set_printer () = 
      let lambda_arg x = 
	match x with
	  Basic.Qnt(q, body) -> (Basic.binder_kind q)=Basic.Lambda
	| _ -> false
      in 
      let printer ppstate (fixity, prec) (f, args) =
	(match args with 
	  (a::rest) -> 
	    let set_fix, set_prec = set_data
	    in 
	    let (qnts, body) = 
	      Term.strip_fun_qnt 
		set_id (Term.mk_app (Term.mk_var set_id) a) []
	    in 
	    Format.printf "@[<2>";
	    (if(lambda_arg a)
	    then 
	      (Format.printf "%s" ocb_sym;
	      Term.print_qnt_body ppstate (set_fix, set_prec) (qnts, body);
	      Format.printf "%s" ccb_sym)
	    else 
	      Term.simple_print_fn_app 
		ppstate (set_fix, set_prec) (f, args));
	    Printer.print_list 
	      (Term.print_term ppstate 
		 (fixity, prec), Printer.print_space) rest;
	    Format.printf "@]"
	| _ ->
	    Format.printf "@[";
	    Term.print_var_as_identifier ppstate (fixity, prec) f;
	    Format.printf "@]")
      in 
      printer

    let empty_set_printer ppstate prec (f, args) = 
      Format.printf "@[<2>";
      Format.printf "%s%s" ocb_sym ccb_sym;
      Printer.print_list 
	(Term.print_term ppstate prec, Printer.print_space) args;
      Format.printf "@]"

    let single_set_printer ppstate prec (f, args) = 
      match args with 
	(a::rest) -> 
	  Format.printf "@[<2>";
	  Format.printf "%s" ocb_sym;
	  Term.print_term ppstate prec a;
	  Format.printf "%s" ccb_sym;
	  Printer.print_list 
	    (Term.print_term ppstate prec, Printer.print_space) rest;
	  Format.printf "@]"
      | _ -> 
	  Format.printf "@[<2>";
	  Term.print_var_as_identifier ppstate prec f;
	  Format.printf "@]"

    let init_set_printer()=
      let set_print = set_printer()
      in 
      Global.PP.add_term_printer set_id set_print;
      Global.PP.add_term_printer single_id single_set_printer

(*
	(fun ppstate i -> set_print ppstate (Printer.nonfix i));
	(fun ppstate i -> single_set_printer ppstate (Printer.nonfix, i))
*)
  end

open SetPP;;

let _ = SetPP.init_set_parser();;
let _ = SetPP.init_set_printer();;
