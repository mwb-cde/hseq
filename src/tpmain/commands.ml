open Result

(* Infixes *)

type fixity = Parserkit.Info.fixity
let nonfix=Parserkit.Info.nonfix 
let prefix=Parserkit.Info.prefix
let suffix=Parserkit.Info.suffix
let infixl=Parserkit.Info.infix Parserkit.Info.left_assoc
let infixr=Parserkit.Info.infix Parserkit.Info.right_assoc
let infixn=Parserkit.Info.infix Parserkit.Info.non_assoc


let catch_errors f a =
  (try f a 
  with 
    Result.Error e -> 
      Result.print_error (Tpenv.pp_info()) (-1) (Result.Error e); 
      raise(Result.Error e)
  | x -> raise x)

let theories () = Tpenv.get_theories()

let read x = catch_errors Tpenv.read x

let curr_theory () = (Tpenv.get_cur_thy())
let get_theory_name thy = Theory.get_name (curr_theory())

let save_theory thy prot= 
  if not (Theory.get_protection thy)
  then 
    (let fname = Filename.concat
	(Tpenv.get_cdir()) ((get_theory_name thy)^(Tpenv.thy_suffix))
    in let oc = open_out fname
    in 
    Theory.export_theory oc thy prot;
    close_out oc)
  else raise (Result.error ("Theory "^(Theory.get_name thy)^" is protected"))

let load_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Tpenv.find_thy_file fname
  in 
  ignore(Thydb.load_theory (theories()) n false 
	   Tpenv.on_load_thy Tpenv.find_thy_file Tpenv.build_thy_file)

let load_parent_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in 
  ignore(Thydb.load_theory (theories()) n true 
	   Tpenv.on_load_thy Tpenv.find_thy_file Tpenv.build_thy_file)

let load_theory_as_cur n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Tpenv.find_thy_file fname
  in let imprts=
    (Thydb.load_theory (theories()) n false Tpenv.on_load_thy 
       Tpenv.find_thy_file Tpenv.build_thy_file)
  in 
  (Tpenv.set_cur_thy (Thydb.get_thy (theories()) n);
   Thydb.add_importing imprts (theories()))

let parents ns = 
  List.iter load_parent_theory ns;
  Theory.add_parents ns (curr_theory());
  Thydb.add_importing (Thydb.mk_importing (theories())) (theories())

let begin_theory n parents= 
  if n = "" 
  then (raise (Result.error "No theory name"))
  else 
    let thy = (Theory.mk_thy n)
    in 
    let importing=
      try
	((Tpenv.get_base_name())::parents)
      with Not_found -> parents
    in 
    Tpenv.set_cur_thy thy;
    List.iter load_parent_theory importing;
    Theory.add_parents importing thy;
    Thydb.add_importing importing (theories())

let new_theory n = begin_theory n

let open_theory n =
  if n = "" 
  then (raise (Result.error "No theory name"))
  else (load_theory_as_cur n)

let close_theory() = 
  if get_theory_name() = "" 
  then (raise (Result.error "At base theory"))
  else save_theory (curr_theory()) false

let end_theory() = 
  if get_theory_name() = "" 
  then (raise (Result.error "At base theory"))
  else save_theory (curr_theory()) true


let add_pp_rec selector id rcrd=
  Thydb.add_pp_rec selector (Basic.name id) rcrd (theories());
  if(selector=Basic.fn_id)
  then Tpenv.add_term_pp_record id rcrd
  else Tpenv.add_type_pp_record id rcrd
    
let add_term_pp id prec fx repr=
  let rcrd=Printer.mk_record prec fx repr
  in 
  add_pp_rec Basic.fn_id id rcrd

let add_type_pp id prec fx repr=
  let rcrd=Printer.mk_record prec fx repr
  in 
  add_pp_rec Basic.type_id id rcrd

let remove_pp_rec selector id =
  Thydb.remove_pp_rec selector 
    (Basic.thy_of_id id) (Basic.name id) (theories());
  if(selector=Basic.fn_id)
  then Tpenv.remove_term_pp id
  else Tpenv.remove_type_pp id 

let remove_term_pp id = remove_pp_rec Basic.type_id id
let remove_type_pp id = remove_pp_rec Basic.type_id id

let get_pp_rec selector id=
  if(selector=Basic.fn_id)
  then Tpenv.get_term_pp id
  else Tpenv.get_type_pp id 

let get_term_pp id=get_pp_rec Basic.fn_id id
let get_type_pp id=get_pp_rec Basic.type_id id

let new_type ?pp (n, args, def) = 
  let trec = Logic.Defns.mk_typedef (Tpenv.scope()) n args def 
  in 
  Thydb.add_type_rec trec (theories());
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> 
      let lname = Basic.mk_long (Tpenv.get_cur_name()) n
      in 
      add_type_pp lname prec fx repr)

(*
let new_type ?pp st = 
  let (n, args, def)= Tpenv.read_type_defn st
  in 
  new_type_term ?pp:pp(n, args, def)
*)

(*
  let trec = Logic.Defns.mk_typedef (Tpenv.scope()) n args def 
  in 
  Thydb.add_type_rec trec (theories())
*)
(*
   [dest_defn_term trm]

   for a term [trm] of the form [f a1 a2 ... an = r] where [n>=0]
   return (f, [a1; a2; ...; an], r)

   for all other terms, raise Failure.
*)
let dest_defn_term trm=
  let err()= failwith "Badly formed definition"
  in 
  if Logicterm.is_equality trm 
  then
    let (lhs, rhs)=Logicterm.dest_equality trm
    in 
    let (f, args) =
      if(Term.is_fun lhs)
      then Term.dest_fun lhs
      else 
	if(Term.is_var lhs)
	then (Term.get_var_id lhs, [])
	else err()
    in 
    let rargs=List.map Term.dest_var args
    in
    (Basic.name f, (List.map (fun (x, y) -> (Basic.name x), y) rargs), rhs)
  else err()
    
let define ?pp ((name, args), r)=
  let ndef=
    Defn.mk_defn (Tpenv.scope()) 
      (Basic.mk_long (Tpenv.get_cur_name()) name) args r
  in 
  let (n, ty, d)= Defn.dest_defn ndef
  in 
  Thydb.add_defn (Basic.name n) ty d (theories()); 
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> add_term_pp n prec fx repr);
  ndef

(*
let define_full trm pp=
  let ndef=define trm 
  in 
  let (n, ty, d)= Defn.dest_defn ndef
  in 
  (let (prec, fx, repr) = pp
  in 
  add_term_pp n prec fx repr); 
  ndef
*)

let declare ?pp trm = 
  let n, ty=
    try 
      (let (v, ty)=Term.dest_typed trm
      in 
      let id =
	if(Term.is_free v)
	then (Basic.mk_long 
		(Tpenv.get_cur_name()) 
		(Term.get_free_name v))
	else Term.get_var_id v
      in 
      let dcl=Defn.mk_decln (Tpenv.scope()) id ty
      in 
      Thydb.add_decln dcl (theories());
      (id, ty))
    with _ -> raise (Result.error ("Badly formed declaration"))
  in 
  match pp with 
    None -> (n, ty)
  | Some(prec, fx, repr) ->
      let longname = 
	if (Basic.thy_of_id n) = Basic.null_thy 
	then 
	  (Basic.mk_long (Tpenv.get_cur_name()) (Basic.name n))
	else n
      in 
      add_term_pp longname prec fx repr;
      (n, ty)
      

(*
let declare_full trm pp =
  let n, ty=declare trm
  in 
  let (prec, fx, repr) = pp
  in 
  let longname = 
    if (Basic.thy_of_id n) = Basic.null_thy 
    then 
      (Basic.mk_long (Tpenv.get_cur_name()) (Basic.name n))
    else n
  in 
  add_term_pp longname prec fx repr;
  (n, ty)
*)

(*
let declare_string str = 
  let t=Tpenv.read_unchecked str
  in 
  try 
    (let (v, ty)=Term.dest_typed t
    in let (n, _)=Term.dest_var v
    in 
    let dcl=Defn.mk_decln (Tpenv.scope()) n ty
    in 
    Thydb.add_decln dcl (theories());
    (n, ty))
  with _ -> raise (Result.error ("Badly formed declaration: "^str))

let declare_full_string str pp =
  let n, ty=declare_string str 
  in 
  let (prec, fx, repr) = pp
  in 
  let longname = 
    if (Basic.thy_of_id n) = Basic.null_thy 
    then 
      (Basic.mk_long (Tpenv.get_cur_name()) (Basic.name n))
    else n
  in 
  add_term_pp longname prec fx repr;
  (n, ty)
*)
let new_axiom n trm =
  let t = Logic.mk_axiom 
      (Formula.form_of_term (Tpenv.scope()) trm)
  in Thydb.add_axiom n t (theories()); t

(*
let new_axiom_string n str =
  new_axiom n (Tpenv.read str)
*)
(*
  let t = Logic.mk_axiom 
      (Formula.form_of_term (Tpenv.scope()) (Tpenv.read str))
  in Thydb.add_axiom n t (theories()); t
*)

let axiom id =
  let t, n = Tpenv.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_axiom t n thys

let theorem id =
  let t, n = Tpenv.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_theorem t n thys

let defn id =
  let t, n = Tpenv.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_defn t n thys

let lemma id =
  let t, n = Tpenv.read_identifier id
  in 
  let thys=theories()
  in 
  Thydb.get_lemma t n thys

let qed n = 
  let t = Goals.result() 
  in 
  Thydb.add_thm n (Goals.result()) (theories()); t

let prove_theorem n t tacs =
  catch_errors
    (fun x -> 
      let nt = Goals.by_list t tacs
      in 
      (Thydb.add_thm n nt x); nt)
    (theories())

(*
let prove_theorem_string n st tacs =
  prove_theorem n (Tpenv.read st) tacs
*)

let save_theorem n th =
  catch_errors 
    (fun x -> Thydb.add_thm n th x; th) (theories())

let by x = 
  (catch_errors Goals.by_com) x

let scope () = Tpenv.scope();;

let read x= Tpenv.read x
let read_unchecked  x= Tpenv.read_unchecked x
let read_defn  x= Tpenv.read_defn x
