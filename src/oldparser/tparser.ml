

module TpParsing=Parsers.Parsers(struct type tokens=Termlex.tok end)


module type TPPARSER =
  sig

    type infotyp =
      { scope: Corepp.pp_state;
	typ_indx: int ref;
        typ_names: (string, Gtypes.gtype) Lib.substype;
        prec_of: Basic.id_selector -> Basic.fnident -> int }

    val string_of_tok : Termlex.tok -> string
    val string_tokens : Termlex.tok list -> string

    val mk_empty_inf : Corepp.pp_state -> infotyp
    val mk_inf : Corepp.pp_state -> infotyp

    val add_type_parser :
      (infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list) 
      ->  unit
    val other_type_parsers :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val add_parser :
      (infotyp -> TpParsing.token list -> Term.term * TpParsing.token list) 
      ->  unit
    val other_parsers :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list

(* atomic and formula parsers *)

    val alternates : ('a -> 'b -> 'c) list -> 'a -> 'b -> 'c

    val error : Termlex.tok list -> 'a
    val none : Termlex.tok list -> Termlex.tok * 'a list

    val id : Termlex.tok list -> Basic.fnident * Termlex.tok list

(*
    val number : Termlex.tok list -> int * Termlex.tok list
*)
    val number : Termlex.tok list -> Num.num * Termlex.tok list
    val boolean : Termlex.tok list -> bool * Termlex.tok list

    val bool_type : Termlex.tok list -> Gtypes.gtype * Termlex.tok list
    val num_type : Termlex.tok list -> Gtypes.gtype * Termlex.tok list

    val comma_list :
      (TpParsing.token list -> 'a * TpParsing.token list) ->
      TpParsing.token list -> 'a list * TpParsing.token list
    val listof :
      (TpParsing.token list -> 'a * TpParsing.token list) ->
      TpParsing.token list -> 'a list * TpParsing.token list

    val short_id :
      'a -> TpParsing.token list ->  string * TpParsing.token list
    val long_id : 'a -> TpParsing.token list -> 
      Basic.fnident * TpParsing.token list

    val types :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val inner_types :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list
    val atomic :
      infotyp -> TpParsing.token list -> Gtypes.gtype * TpParsing.token list

    val typedef :
      infotyp ->
      TpParsing.token list ->
      (string * string list option * Gtypes.gtype option) *
      TpParsing.token list


    val form :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val formula :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list

    val possible_type :
      infotyp ->
      TpParsing.token list -> Gtypes.gtype option * TpParsing.token list
    val typed_primary :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val primary :
      infotyp -> TpParsing.token list -> Term.term * TpParsing.token list
    val id_type_op :
      (infotyp -> TpParsing.token list 
	-> 'a * TpParsing.token list) ->
	  infotyp ->
      TpParsing.token list -> ('a * Gtypes.gtype) * TpParsing.token list

    val defn :
      infotyp ->
      TpParsing.token list ->
      ((string * (string * Gtypes.gtype) list) * Term.term) *
      TpParsing.token list

    val defeq :
      infotyp ->
      TpParsing.token list ->
      ((string * (string * Gtypes.gtype) list) * Term.term) *
      TpParsing.token list

    val read_typedef :
      Corepp.pp_state ->
      string -> string * string list option * Gtypes.gtype option

    val read_defeq :
      Corepp.pp_state ->
      string -> (string * (string * Gtypes.gtype) list) * Term.term

    val identifier : Corepp.pp_state -> string -> Basic.fnident


    val read_type : Corepp.pp_state -> string -> Gtypes.gtype
    val read_fulltype : Corepp.pp_state -> string -> Gtypes.gtype
    val read_term : Corepp.pp_state -> string -> Term.term
    val read : Corepp.pp_state -> string -> Term.term
  end

module TpParser : TPPARSER =
struct

  open Termlex
  open Termlexer
  open TpParsing
  open Term

  type infotyp = 
      { scope: Corepp.pp_state;
      	typ_indx : int ref;
	typ_names: (string, Gtypes.gtype)Lib.substype;
  	prec_of: Basic.id_selector -> Basic.fnident -> int}

  let string_of_tok tok = Termlex.string_of_token tok

  let string_tokens toks =
    Lib.list_string string_of_tok " " toks

  let get_type_indx inf = 
    inf.typ_indx:= (!(inf.typ_indx))+1; !(inf.typ_indx)

  let mk_vartyp inf = 
    Gtypes.mk_var
      ("_typ"^(string_of_int (get_type_indx inf)))

  let lookup n inf = Lib.find n inf.typ_names
  let add_name n ty inf = Lib.add n ty inf.typ_names
  let get_type n inf = 
    (try lookup n inf 
    with Not_found -> 
      add_name n (Gtypes.mk_var n) inf)

  let clear_names inf = Hashtbl.clear inf.typ_names
  let mk_empty_inf scp = 
    { scope= scp;
      typ_indx = ref 0;
      typ_names = Lib.empty_env ();
      prec_of= fun x y -> (-1)}

  let mk_inf scp = 
    { scope=scp;
      typ_indx = ref 0;
      typ_names = Lib.empty_env ();
      prec_of= 
      (fun idsel x -> Corepp.prec_of scp idsel x) }

  let scope_of_inf inf = Gtypes.empty_scope() (* inf.scope*)

  let rec alternates phl inf toks= 
    match phl with
      [] -> raise (ParsingError "No alternative parsers")
    | (ph::phs) -> 
	(try (ph inf) toks 
	with ParsingError _ -> (alternates phs inf toks))

  let error toks =
    raise (ParsingError (string_tokens toks))

  let other_type_parsers_list  = ref [(fun x-> error)]
  let add_type_parser ph = 
    other_type_parsers_list:=(ph::!other_type_parsers_list)
  let other_type_parsers inf toks = 
    alternates (!other_type_parsers_list) inf toks

  let other_parsers_list  = ref [(fun x-> error)]
  let add_parser ph = 
    other_parsers_list:=(ph::!other_parsers_list)
  let other_parsers inf toks = 
    alternates (!other_parsers_list) inf toks

  let prec_of inf t= 
    match t with 
    ID s -> inf.prec_of Basic.fn_id s
    | Key ALL -> 0
    | Key EX -> 0
    | Key LAM -> 0
   | _ -> -1

  let type_prec inf t = 
    match t with
      Sym RIGHTARROW -> 6
    | ID(s) -> inf.prec_of Basic.type_id s
    | _ -> -1

  let mktype_constr t=
    match t with
      Sym RIGHTARROW -> Gtypes.mk_fun
    | ID(s) -> (fun x y -> Gtypes.mk_def s [x;y])
    | _ -> raise (ParsingError 
		    ((string_of_tok t)^" is not a type constructor"))

  let mk_conn idsel inf  t= 
    match t with 
     ID(i) -> (if (Corepp.is_infix inf.scope idsel i)
	 then (fun x y -> Term.mkfun i [x; y])
	 else raise (ParsingError ((string_of_tok t)^" is not a connective")))
    | _ -> raise (ParsingError ((string_of_tok t)^" is not a connective"))

  let id toks= 
    match toks with
      (ID(s)::ts) -> (s, ts)
    | _ -> raise (ParsingError "Empty input")

  let number toks = 
    match toks with
      (NUM(n)::ts) ->
       	(try (Num.num_of_string n, ts)
	  with _ -> raise (ParsingError "Not a number"))
    | ts -> raise (ParsingError "Not a number")

(*
  let number toks = 
    match toks with
      (NUM(n)::ts) ->
       	(try (int_of_string n, ts)
	  with _ -> raise (ParsingError "Not a number"))
    | ts -> raise (ParsingError "Not a number")
*)
  let boolean toks = 
    match toks with
      (BOOL(b)::ts) ->
       	(b, ts)
    | ts -> raise (ParsingError "Not a boolean")

  let none toks =
    match toks with 
      [] -> (ID (Basic.mkname ""), [])
    | _ -> raise (ParsingError (string_tokens toks))


  let bool_type toks =
    match toks with
      ((ID ("", "bool"))::ts) -> (Gtypes.mk_bool, ts)
    | ts -> raise (ParsingError "Not a boolean type")

  let num_type toks =
    match toks with
      ((ID ("", "num"))::ts) -> (Gtypes.mk_num, ts)
    | ts -> raise (ParsingError "Not an number type")


  let comma_list ph toks= 
    ( ((ph -- (repeat ((Sym COMMA) $-- ph))) 
	 >> (fun x -> (fst x)::(snd x)))
    ||  empty >> (fun x -> [])) toks

  let listof ph toks=  repeat ph toks
      
  let rec repeat_term ph term toks =
      (((ph -- (repeat_term ph term )) >> (fun (x, y) -> x ::y))
     || (term >> (fun _ -> []))) toks

  let tlistof ph tm toks= 
    ( ((ph -- (repeat_term ph tm))
	 >> (fun (x, y) -> (x::y)))
    || (tm >> (fun x -> []))) toks
(*
    ||  (empty >> (fun x -> []))) toks
*)
  let short_id inf toks =
    ((id >> 
      (fun x -> 
	match (Basic.dest_fnid x) with 
	  ("", s) -> s | _ -> raise (ParsingError "Not a short identifier"))) 
       toks)

  let long_id inf toks = 
      ((id >> 
      (fun x -> 
      match (Basic.dest_fnid x) with 
      (_, "") -> raise (ParsingError "Badly formed identifier")
      | _ -> x))
      toks)

  let mk_short_id inf toks =
    (long_id inf >> (fun x -> Basic.name x)) toks

  let rec types inf toks = 
    (clear_names inf; inner_types inf toks)
  and inner_types inf toks =
    (infixes (atomic inf, type_prec inf, mktype_constr) toks)
  and atomic inf toks =
    ((
      ((!$(Sym ORB) -- ((inner_types inf) -- !$(Sym CRB)))
	>> (fun x -> fst (snd x)))
    || (( num_type) >> (fun x -> x))
    || ((bool_type >> (fun x -> x)))
    || ((!$(Sym PRIME) -- short_id inf )
	      >> (fun (_, x) -> get_type x inf))
    || (((long_id inf) -- 
	(optional 
	   ((!$(Sym ORB) -- ((comma_list (inner_types inf))
			       -- (!$(Sym CRB))))
	      >> (fun (_, (args, _)) -> args))))
	>> (fun (i, a) -> 
	     match a with
	       None -> Gtypes.mk_def i []
	     | Some(ts) -> Gtypes.mk_def i ts))
    || (other_type_parsers inf)
    || error)
      toks)

  let typedef inf toks = 
    let type_id inf toks =
      ((!$(Sym PRIME) -- (short_id inf))>> (fun (_, x) -> x)) toks
    in 
    (((short_id inf) 
       -- 
       ((optional 
	  ((!$(Sym ORB)-- ((comma_list (type_id inf)) -- (!$(Sym CRB))))
	     >> (fun (_, (x, _)) -> x)))
       -- 
       (optional 
	  (((!$(ID Logicterm.equalsid))
	      -- (types inf)) >> (fun (_, x) -> x)))))
      >> (fun (x, (y, z)) -> (x, y, z))) toks

  let rec mkcomb x y = 
    match y with 
      [] -> x
    | t::ts -> mkcomb (mkapp x t) ts

  let id_type_op idnt inf toks=
    (( (((!$(Sym ORB)) -- (idnt inf) 
	   -- (!$(Sym COLON))-- (types inf) -- (!$(Sym CRB)))
	  >> (fun ((((_, i), _), t), _) -> (i, t)))
  || ( (idnt inf) >> (fun x -> (x, mk_vartyp inf))))
    toks)


let possible_type inf toks =
      ( (( !$(Sym COLON) -- (types inf)) >> (fun (_, ty) -> Some(ty)))
      || (empty >> (fun x -> None))
      ) toks

let possible ph toks =
  ( ( ph >> (fun x -> Some x))
  || (empty >> (fun x -> None))
      ) toks

let rec form inf toks =
 (
 ((formula inf)-- (listof (formula inf)))
    >> (fun (x, y) -> mkcomb x y)
) toks
and formula inf toks= 
(
  infixes(typed_primary inf, prec_of inf, mk_conn Basic.fn_id inf)
)
 toks
and 
typed_primary inf toks =
  ( (((primary inf) --  (possible_type inf))
       >> 
     (fun (t, pty) -> 
       match pty with None -> t | Some(ty) -> mktyped t ty))
      toks)
and
primary inf toks = 
  (
     ( (( !$ (Sym ORB) -- ((form inf) -- !$(Sym CRB))))
	  >> (fun (_, (x, _)) ->  x))
  || (( !$ (Key ALL) 
        -- ((id_type_op short_id inf)
	-- ((listof (id_type_op short_id inf))
	      -- (!$(Sym COLON) -- (form inf)))))
	>> (fun (_, (v, (vs, (_, b)))) -> 
	  (Logicterm.mkall_ty (scope_of_inf inf) (fst v) (snd v) 
	     (List.fold_right (fun (x, y) -> 
	       Logicterm.mkall_ty (scope_of_inf inf) x y) vs b))))
    ||
    (( !$ (Key EX) 
        -- ((id_type_op short_id inf)
	-- ((listof (id_type_op short_id inf))
	      -- (!$(Sym COLON) -- (form inf)))))
	>> (fun (_, ((v, vt), (vs, (_, b)))) -> 
	  (Logicterm.mkex_ty (scope_of_inf inf) v vt 
	     (List.fold_right 
		(fun (x, y) -> Logicterm.mkex_ty (scope_of_inf inf) x y) 
		vs b))))
    || (( !$ (Key LAM) 
        -- ((id_type_op short_id inf)
	-- ((listof (id_type_op short_id inf))
	      -- (!$(Sym COLON) -- (form inf)))))
	>> (fun (_, ((v, vt), (vs, (_, b)))) -> 
	  (Logicterm.mklam_ty (scope_of_inf inf) v  vt 
	     (List.fold_right 
		(fun (x, y) -> Logicterm.mklam_ty (scope_of_inf inf)x y) 
		vs b))))
    ||  (number >> (fun x -> mknum x))
      || (boolean >> (fun x -> mkbool x))
      || ((id_type_op long_id inf)  >> 
         (fun ((n, i), t) -> mk_typed_var (Basic.mklong n i) t))
      || (other_parsers inf)
      ||(error)
	) toks


    let rec lhs inf toks=
      ((( (id_type_op mk_short_id inf) -- (args_opt inf))
	 >> (fun ((n, t) , args) -> (n, args))) toks)
    and args_opt inf toks= 
      (tlistof (id_type_op short_id inf) (!$(ID Logicterm.equalsid))) toks
    and defn inf toks =
      ( (( ((lhs inf) -- (form inf))
	    >> (fun (l,  r) -> (l, r)))
      || error) toks)
  let defeq inf toks = 
     defn inf toks


  let fulltypedef inf toks =
    (((typedef inf) -- none) >> (fun (x, _) -> x)) toks
  let read_typedef pr x = fst (fulltypedef (mk_inf pr) (Termlexer.Lex.lex x))

  let fulldefeq inf toks = 
    (((defeq inf) -- none) >> (fun (x, _) -> x)) toks
  let read_defeq pr x = fst ((fulldefeq (mk_inf pr)) (Termlexer.Lex.lex x))

  let fulltype inf toks =
    (((types inf) -- none)>> (fun x -> fst x)) toks

  let fullform inf toks = 
    (((form inf) -- none)>> (fun x -> fst x)) toks

  let parse_fulltype pr x = fst(fulltype (mk_inf pr) x)
  let parse_type pr x = fst(types (mk_inf pr) x)

  let identifier scp x = 
    let (t, n)=fst(long_id (mk_empty_inf scp) (Termlexer.Lex.lex x))
    in Basic.mklong t n
    
  let parse_fullterm pr x = fst(fullform (mk_inf pr) x)
  let parse_term pr x = fst(form (mk_inf pr) x)

  let read_type pr x = parse_type pr (Termlexer.Lex.lex x)
  let read_fulltype pr x = parse_fulltype pr (Termlexer.Lex.lex x)

  let read_term pr x= parse_term pr (Termlexer.Lex.lex x)
  let read pr x= parse_fullterm pr (Termlexer.Lex.lex x)
end;;
