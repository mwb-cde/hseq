open Format
exception Error of string

type pp_rec =
    {
    prec: int;            (* its precedence *)
    infix: bool;          (* it is infix *)
    repr: string option   (* its representation *)
  } 

type pp_state = 
    { id_info: Basic.fnident -> pp_rec;
      type_info: Basic.fnident -> pp_rec}

type pp_info = (Basic.fnident * pp_rec) list

let mk_pp_rec pr inf rep =
  {prec=pr; infix = inf; repr= rep }

let empty_pp_rec () = mk_pp_rec (-1) false None

let empty_pp_info = []
let get_pp name pp_ls = Lib.assoc (fun (x, _) -> x=name) pp_ls
let add_pp id pp_rec pp_ls = ((id, pp_rec)::pp_ls)
let remove_pp p pp_ls = Lib.filter p pp_ls

let type_pp_info = ref []    (* info for types *)
let id_pp_info = ref []     (* info for identifiers *)

let get_id_info name = get_pp name (!id_pp_info)
let add_id_info id pp_rec=
  try (ignore(get_id_info id);
       raise (Error ((Basic.string_fnid id)^" already exists")))
  with Not_found -> (id_pp_info:=add_pp id pp_rec (!id_pp_info))
let remove_id_info p = 
  id_pp_info := remove_pp p (!id_pp_info)

let get_type_info name = get_pp name (!id_pp_info)
let add_type_info id pp_rec=
  try (ignore(get_type_info id);
       raise (Error ((Basic.string_fnid id)^" already exists")))
  with Not_found -> (id_pp_info:=add_pp id pp_rec (!id_pp_info))
let remove_type_info p = 
  type_pp_info := remove_pp p (!type_pp_info)

let base_id_info = []          (* standard identifiers and their pp info *)
let base_type_info = []        (* standard types and their pp info *)

    (* rebuild base identifier and type pp info *)
let build_id_info () = id_pp_info:=base_id_info
let build_type_info () = type_pp_info:=base_id_info

let mk_empty_pp_state () =
  { id_info = (fun _ -> empty_pp_rec());
    type_info = (fun _ -> empty_pp_rec())}

let mk_base_pp_state () =
  { id_info = (fun x -> snd (get_id_info x)); 
    type_info = (fun x -> snd (get_type_info x)) }

let prec_of st sel id = 
  let pp_rec= 
    if sel = Basic.fn_id 
    then st.id_info id else st.type_info id
  in pp_rec.prec

let is_infix st sel id = 
  let pp_rec=
    if sel = Basic.fn_id 
    then st.id_info id else st.type_info id
  in pp_rec.infix


(* pretty printing functions *)

(*
let rec string_gtype x =
  match x with
      	Types.Var(a) -> "'"^(!a)
      | Types.Constr(f, args) ->  
	  Basic.string_tconst f (List.map string_gtype args)
      | Types.Base(b) -> Basic.string_btype b

*)
  let rec list_print f sep x =
    match x with 
      [] -> ()
    | (b::[]) -> (f b)
    | (b::bs) -> (f b); sep(); (list_print f sep bs)

let string_identifier id pp_rec =
  match pp_rec.repr
  with 
    None -> (Basic.string_fnid id) 
  | Some(x) -> x

(*
let print_type_info ppstate pr t = 
  let rec print_aux old_pr x =
    match x with
      Types.Var(_) -> print_string ("'"^(Types.get_var x))
    | Types.Base(b) -> print_string (Basic.string_btype b)
    | Types.Constr(Basic.Defined n, [a1; a2])  (* possible infix *)
      ->
	(let pp_rec=(try ppstate.type_info n with _ -> empty_pp_rec ())
	in 
	let infix=pp_rec.infix 
	and prec = pp_rec.prec
	in 
 	let brckt=prec<old_pr
	in 
	(open_box 0;
	 if infix 
	 then (if brckt then print_string "(" else ();
	       print_aux prec a1; 
	       print_string 
		 (" "^(string_identifier n pp_rec)^" ");
	       print_aux prec a2;
	       if brckt then print_string ")" else ())
	 else (print_string 
		 (string_identifier n pp_rec);
	       print_string "(";
	       list_print (print_aux prec) (fun _ -> ", ") [a1;a2];
	       print_string ")");
	 close_box ()))
    | Types.Constr(Basic.Defined n, args) ->  (* not infix *)
	(let pp_rec=(try ppstate.type_info n with _ -> empty_pp_rec ())
	in 
	let prec=pp_rec.prec
	in 
	(open_box 0;
	 print_string (string_identifier n pp_rec);
	 print_string "(";
	 list_print (print_aux prec ) (fun _ -> ", ") args;
	 print_string ")";
	 close_box ()))
    | Types.Constr(Basic.Func, [a1; a2]) ->  (* not infix *)
	open_box 0;
	print_aux 0 a1; 
	print_string "->";
	print_aux 0 a2;
	close_box()
    | Types.Constr(Basic.Func, args) ->  (* not infix *)	      
	(open_box 0;
	 print_string "->";
	 print_string "(";
	 list_print (print_aux 0) (fun _ -> ", ") args;
	 print_string ")";
	 close_box ())
  in print_aux pr t

let print_type st x =open_box 0; print_type_info st 0 x; close_box ()
*)
(*
  open_box 0; print_string(Types.string_gtype x); close_box()
*)
(*
  let rec print_term_aux ppstate i x =
    match x with
      Term.Var(n, ty) -> print_string(Basic.string_fnid n)
    | Term.Bound(n) -> print_string ("'"^(Term.get_binder_name x))
    | Term.Const(c) -> print_string (Basic.string_const c)
    | Term.Typed (trm, ty) ->
	print_string "(";
	open_box 0;
	print_term_aux ppstate i trm;
	close_box();
	print_string ": ";
	print_string (Types.string_gtype ty);
	print_string ")"
    | Term.App(t1, t2) ->
	let f=Term.get_fun x 
	and args = Term.get_args x
	in 
	if Term.is_var f 
	then 
	  (let name= fst (Term.dest_var f)
	  in 
	  let pp_rec = try ppstate.id_info name with _ -> empty_pp_rec ()
	  in 
	  let pr = pp_rec.prec
	  in let ti = if pr <i then pr else i
	  in if pp_rec.infix
	  then 
	    (match args with
	      [l;r] -> 
		(print_term_aux ppstate ti l;
		 print_break 1 2;
		 print_term_aux ppstate ti f;
		 print_break 1 2;
		 print_term_aux ppstate ti r)
	    | _ -> (print_term_aux ppstate ti f;
		    print_break 1 2;
		    list_print 
		      (print_term_aux ppstate ti)
		      (fun () -> print_break 1 2)
		      args))
	  else 
	    (print_string "(";
	     print_term_aux ppstate ti f;
	     print_break 1 2;
	     list_print 
	       (print_term_aux ppstate ti)
	       (fun () -> print_break 1 2) args;
	     print_string")"))
	else 
	    (print_string "(";
	     print_term_aux ppstate i  f;
	     print_break 1 2;
	     list_print 
	       (print_term_aux ppstate i)
	       (fun () -> print_break 1 2) args;
	     print_string")")
    | Term.Qnt(q, body) -> 
	let (_, qnt, qvar, qtyp, _) = Term.dest_qnt x
	in let (qnts, b) = (Term.strip_qnt qnt x)
	in let qnts_str = 
	  (Basic.quant_string qnt)
	  ^(Lib.list_string 
	      (fun x -> (Term.string_typed_name (!x.Term.qvar) (!x.Term.qtyp)))
	      " " qnts)^": "
	in 
	let ti = (Basic.prec_qnt (qnt))
	in 
	if ti < i 
	then
	  (print_string "(";
	   print_string qnts_str;
	   open_box 0;
	   print_term_aux ppstate ti b;
	   close_box();
	   print_string ")")
	else 
	   print_string qnts_str;
	   open_box 0;
	   print_term_aux ppstate ti b;
	   close_box()


  let  print_term ppstate x = 
    open_box 0;
    print_term_aux ppstate 0 x;
    close_box()
*)
