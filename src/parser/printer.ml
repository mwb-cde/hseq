module Info =
struct

  open Format
  exception Error of string

(* Fixity *)
(* associativity of infix is not important in the printer *)

  type fixity=Parserkit.Info.fixity
  let nonfix=Parserkit.Info.nonfix 
  let prefix=Parserkit.Info.prefix
  let suffix=Parserkit.Info.suffix
  let infix=Parserkit.Info.infix Parserkit.Info.non_assoc

  type pp_rec =
      {
       prec: int;            (* its precedence *)
       fixity: fixity; 
       repr: string option   (* its representation *)
     } 

  type pp_state = 
      { id_info: Basic.fnident -> pp_rec;
	type_info: Basic.fnident -> pp_rec}

  type pp_info = (Basic.fnident * pp_rec) list

  let mk_pp_rec pr inf rep =
    {prec=pr; fixity = inf; repr= rep }

  let empty_pp_rec () = mk_pp_rec (-1) nonfix None

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

  let fixity_of st sel id=
    let pp_rec=
      if sel = Basic.fn_id 
      then st.id_info id 
      else st.type_info id
    in pp_rec.fixity

  let is_infix fx=
    Parserkit.Info.is_infix fx

  let is_prefix fx =
    Parserkit.Info.is_prefix fx

  let is_suffix fx =
    Parserkit.Info.is_suffix fx
end

open Term
open Format
open Result
open Info

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


let printer_info_list = ref []
let printer_info () = !printer_info_list
let set_printer_info l = printer_info_list := l

let prec_of f = 
  try fst (List.assoc f (printer_info()))
  with Not_found -> Parser.default_term_prec

let fixity_of f =
  try snd (List.assoc f (printer_info()))
  with Not_found -> Parser.default_term_fixity

let type_printer_info_list = ref []
let type_printer_info () = !printer_info_list      
let set_type_printer_info l = type_printer_info_list := l

let add_printer_info st pr inf = 
  try (ignore(List.assoc st (!printer_info_list)));
    failwith ("Identifier "^(Basic.string_fnid st)^" exists")
  with Not_found -> 
    printer_info_list:=(st, (pr, inf))::!printer_info_list

let remove_printer_info st =
  let rec remove_aux ls =
    match ls with
      [] -> []
    |	((x, y)::xs) -> 
	if x=st then remove_aux xs else (x, y)::(remove_aux xs)
  in printer_info_list:=remove_aux (!printer_info_list)


let add_type_printer_info st pr inf = 
  try (List.assoc st (!type_printer_info_list));
    failwith ("Identifier "^st^" exists")
  with Not_found -> 
    type_printer_info_list:=(st, (pr, inf))::!type_printer_info_list


let remove_type_printer_info st =
  let rec remove_aux ls =
    match ls with
      [] -> []
    |	((x, y)::xs) -> if x=st then remove_aux xs else (x, y)::(remove_aux xs)
  in type_printer_info_list:=remove_aux (!type_printer_info_list)



let cfun_string c =
  match c with 
    "not" -> print_string "not"
  | "and" -> print_string "and"
  | "or" -> print_string "or"
  | "implies" -> print_string " => "
  | "iff" -> print_string "<=>"
  | "equals" -> print_string "="
  | x -> print_string x



let print_fnident x = print_string (Basic.string_fnid x)

let print_subst tenv f= 
  open_box 0;
  (Hashtbl.iter 
     (fun x y -> print_string 
	 ("("^(f x)^" = "^(f y)^"):"))
     tenv);
  close_box()

let print_error inf r = (r#print) inf


(* print_bracket: print a bracket depending on relative priority *)

let print_bracket pr i br =
  if pr < i then print_string br else ()

(* Identifier Printer *)
let print_identifier pprec x=
  let str=
    (match pprec.repr with
      None -> Basic.name x
    | Some(s) -> s)
  in 
  Format.print_string str


(* Type printer *)


let get_var t = 
  match t with 
    Gtypes.Var(n) -> !n
  | _ -> raise (Failure "Not a variable")

let print_type_info ppstate pr t = 
  let rec print_aux old_pr x =
    match x with
      Gtypes.Var(_) -> Format.print_string ("'"^(get_var x))
    | Gtypes.Base(b) -> Format.print_string (Basic.string_btype b)
    | Gtypes.Constr(Basic.Defined n, args) ->  (* not infix *)
	print_defined old_pr n args
    | Gtypes.Constr(Basic.Func, args) ->
	print_func args
  and print_defined oldprec f args =
    let print_infix pp_rec pr ls =
      Format.open_box 0;
      (match ls with
	l::rargs ->
	  print_aux pr l;
	  print_identifier pp_rec f;
	  list_print (print_aux pr) (fun _ -> " ") rargs;
      | _ -> 
	  print_identifier pp_rec f;
	  print_string "(";
	  list_print (print_aux pr) (fun _ -> ", ") ls;
	  print_string ")");
      Format.close_box()
    in 
    let pp_rec=ppstate.type_info f
    in 
    let prec,fixity=pp_rec.prec, pp_rec.fixity
    in 
    Format.open_box 0;
    print_bracket prec oldprec "(";
    if(is_infix fixity) or (is_suffix fixity)
    then print_infix pp_rec prec args
    else 
       (print_identifier pp_rec f;
       Format.print_string "(";
       list_print (print_aux pr) (fun _ -> ", ") args;
       Format.print_string ")");
    print_bracket prec oldprec ")";
    Format.close_box()
  and print_func args  =
    match args with
      a1::a2::rst ->
	Format.open_box 0;
	print_aux 0 a1; 
	Format.print_string "->";
	print_aux 0 a2;
	Format.close_box()
    | _ -> 
	Format.open_box 0;
	Format.print_string "->";
	Format.print_string "(";
	list_print (print_aux 0) (fun _ -> ", ") args;
	Format.print_string ")";
	Format.close_box ()
  in 
  print_aux pr t

let print_type st x =
  Format.open_box 0; print_type_info st 0 x; Format.close_box ()


(* Term Printer *)

let string_typed_name n t = 
  ("("^n^": "^(Gtypes.string_gtype t)^")")


let print_qnt q =
  let _, qvar, qtyp = Term.dest_binding q 
  in 
  Format.open_box 0; 
  Format.print_break 1 2;
  Format.print_string (string_typed_name (qvar) (qtyp));
  Format.close_box()

let rec print_term_aux ppstate i x =
  match x with
    Var(n, ty) -> 
      (let pprec = ppstate.id_info n
      in 
      print_identifier pprec n)
  | Bound(n) -> Format.print_string ((get_binder_name x))
  | Const(c) -> Format.print_string (Basic.string_const c)
  | Typed (trm, ty) -> print_typed_term ppstate i trm ty
  | App(t1, t2) ->
      let f, args=get_fun_args x 
      in 
      (match args with 
	[] -> print_term_aux ppstate i f
      | _ -> 
	  (if is_var f 
	  then print_fn_app ppstate i f args
	  else (Format.print_string "(";
		print_term_aux ppstate i  f;
		Format.print_break 1 2;
		list_print 
		  (print_term_aux ppstate i)
		  (fun () -> Format.print_break 1 2) args;
		Format.print_string")")))
  | Qnt(q, body) -> 
      let (_, qnt, qvar, qtyp, _) = dest_qnt x
      in let (qnts, b) = (strip_qnt qnt x)
      in 
      let print_qnts qs =
	Format.print_string (Basic.quant_string qnt);
	(list_print print_qnt 
	   (fun () -> Format.print_break 1 2) qnts);
	Format.print_string ": ";
	Format.print_cut()
      in 
      let ti = (Basic.prec_qnt (qnt))
      in 
      Format.open_box 0; print_bracket ti i "(";
      Format.print_break 0 2; print_qnts qnts;
      Format.close_box();
      Format.open_box 0; print_term_aux ppstate ti b;
      Format.close_box(); print_bracket ti i ")";
      Format.close_box()
and 
    print_fn_app ppstate i f args=
  let print_infix ti args =
    match args with
      (l::rargs) -> 
	(print_term_aux ppstate ti l;
	 Format.print_break 1 2;
	 print_term_aux ppstate ti f;
	 list_print (print_term_aux ppstate ti)
	   (fun () -> Format.print_break 1 2) args)
    | _ -> list_print (print_term_aux ppstate ti)
	  (fun () -> Format.print_break 1 2)
	  args
  in 
  let name= fst (dest_var f)
  in let pp_rec = ppstate.id_info name 
  in let pr, fixity = pp_rec.prec, pp_rec.fixity
  in let ti = if pr <= i then pr else i
  in
  print_bracket pr i "(";
  (if (is_infix fixity) or (is_suffix fixity)
  then print_infix ti args
  else list_print (print_term_aux ppstate ti)
      (fun () -> Format.print_break 1 2) args);
  print_bracket pr i ")"
and 
    print_typed_term ppstate i trm ty=
  Format.open_box 0;
  Format.print_string "(";
  print_term_aux ppstate i trm;
  Format.print_string ": ";
  Format.print_string (Gtypes.string_gtype ty);
(* print_type ppstate ty *)  
  Format.print_string ")";
  Format.close_box()


let print_term inf x = 
  open_box 0;
  Term.print_term_aux inf 0 x;
  close_box()

let rec print_termlist inf x =
  match x with 
    [] -> print_string ""
  | [p] -> print_term inf p
  | (p::ps) -> 
      (print_term inf p; 
       print_string ", "; 
       print_termlist inf ps)


