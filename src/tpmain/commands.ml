(*-----
 Name: commands.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

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
      Result.print_error (Global.pp_info()) (-1) (Result.Error e); 
      raise(Result.Error e)
  | x -> raise x)

let theories () = Global.get_theories()

let read x = catch_errors Global.read x

let curr_theory () = Global.get_cur_thy()
let get_theory_name thy = Theory.get_name (curr_theory())

let theory name = 
   if name = "" 
   then curr_theory()
   else Thydb.get_thy (theories()) name

let save_theory thy prot= 
  let fname = Filename.concat
      (Global.get_cdir()) ((get_theory_name thy)^(Global.thy_suffix))
  in let oc = open_out fname
  in 
  Theory.export_theory oc thy prot;
  close_out oc

let load_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Global.find_thy_file fname
  in 
  ignore(Thydb.load_theory (theories()) n false 
	   Global.on_load_thy Global.find_thy_file Global.build_thy_file)

let load_parent_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in 
  ignore(Thydb.load_theory (theories()) n true 
	   Global.on_load_thy Global.find_thy_file Global.build_thy_file)

let load_theory_as_cur n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Global.find_thy_file fname
  in let imprts=
    (Thydb.load_theory (theories()) n false Global.on_load_thy 
       Global.find_thy_file Global.build_thy_file)
  in 
  (Global.set_cur_thy (Thydb.get_thy (theories()) n);
   Thydb.add_importing imprts (theories()))

let parents ns = 
  List.iter load_parent_theory ns;
  Theory.add_parents ns (curr_theory());
  Thydb.add_importing (Thydb.mk_importing (theories())) (theories())

let add_file f =
  Theory.add_file f (curr_theory())

let remove_file f =
  Theory.remove_file f (curr_theory())

let begin_theory n parents= 
  if n = "" 
  then (raise (Result.error "No theory name"))
  else 
    let importing=
      try
	((Global.get_base_name())::parents)
      with Not_found -> parents
    in 
    List.iter load_parent_theory importing;
    let thy = (Theory.mk_thy n)
    in 
    Global.set_cur_thy thy;
    Theory.add_parents importing thy;
    Thydb.add_importing (Thydb.mk_importing (theories())) (theories())

let new_theory n = begin_theory n

let open_theory n =
  if n = "" 
  then (raise (Result.error "No theory name"))
  else (load_theory_as_cur n)

let close_theory () = 
  if get_theory_name() = "" 
  then (raise (Result.error "At base theory"))
  else 
    (let thy = curr_theory()
    in 
    Theory.end_theory thy false;
    save_theory thy false)

let end_theory ?(save=true) () = 
  if get_theory_name() = "" 
  then (raise (Result.error "At base theory"))
  else 
    (let thy = curr_theory()
    in 
    Theory.end_theory thy true;
    if(save) then save_theory thy true else ())


let add_pp_rec selector id rcrd=
  Thydb.add_pp_rec selector (Basic.name id) rcrd (theories());
  if(selector=Basic.fn_id)
  then Global.add_term_pp_record id rcrd
  else Global.add_type_pp_record id rcrd
      
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
  then Global.remove_term_pp id
  else Global.remove_type_pp id 

let remove_term_pp id = remove_pp_rec Basic.type_id id
let remove_type_pp id = remove_pp_rec Basic.type_id id

let get_pp_rec selector id=
  if(selector=Basic.fn_id)
  then Global.get_term_pp id
  else Global.get_type_pp id 

let get_term_pp id=get_pp_rec Basic.fn_id id
let get_type_pp id=get_pp_rec Basic.type_id id

let new_type ?pp (n, args, def) = 
  let trec = Logic.Defns.mk_typedef (Global.scope()) n args def 
  in 
  Thydb.add_type_rec trec (theories());
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> 
      let lname = Basic.mk_long (Global.get_cur_name()) n
      in 
      add_type_pp lname prec fx repr)

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
      
let define ?pp ?(simp=false) ((name, args), r)=
  let ndef=
    Defn.mk_defn (Global.scope()) 
      (Basic.mk_long (Global.get_cur_name()) name) args r
  in 
  let props = 
    if simp
    then [Theory.simp_property]
    else []
  in 
  let (n, ty, d)= Defn.dest_defn ndef
  in 
  Thydb.add_defn (Basic.name n) ty d props (theories()); 
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> add_term_pp n prec fx repr);
  ndef

let declare ?pp trm = 
  let n, ty=
    try 
      let v, ty=
	match trm with
	  Basic.Free(i, t) -> (i, t)
	| Basic.Id(i, t) -> (Basic.name i, t)
	| Basic.Typed(Basic.Free(i, _), t) -> (i, t)
	| Basic.Typed(Basic.Id(i, _), t) -> (Basic.name i, t)
	| _ -> raise (Failure "Badly formed declaration")
      in 
      let id = Basic.mk_long (Global.get_cur_name()) v
      in 
      let dcl = Defn.mk_decln (Global.scope()) id ty
      in 
      Thydb.add_decln dcl [] (theories()); (id, ty)
    with _ -> raise (Result.error ("Badly formed declaration"))
  in 
  match pp with 
    None -> (n, ty)
  | Some(prec, fx, repr) ->
      let longname = 
	if (Basic.thy_of_id n) = Basic.null_thy 
	then 
	  (Basic.mk_long (Global.get_cur_name()) (Basic.name n))
	else n
      in 
      add_term_pp longname prec fx repr;
      (n, ty)

(*
let declare ?pp trm = 
  let n, ty=
    try 
   (let (v, ty)=Term.dest_typed trm 
      in 
      let id =
	if(Term.is_free v)
	then (Basic.mk_long 
		(Global.get_cur_name()) 
		(Term.get_free_name v))
	else Term.get_var_id v
      in 
      let dcl=Defn.mk_decln (Global.scope()) id ty
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
	  (Basic.mk_long (Global.get_cur_name()) (Basic.name n))
	else n
      in 
      add_term_pp longname prec fx repr;
      (n, ty)
*)

let new_axiom ?(simp=false) n trm =
  let t = Logic.mk_axiom 
      (Formula.form_of_term (Global.scope()) trm)
  and props = if simp then [Theory.simp_property] else []
  in 
  Thydb.add_axiom n t props (theories()); t

let axiom id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_axiom t n thys

let theorem id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_theorem t n thys

let defn id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_defn t n thys

let lemma id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in 
  Thydb.get_lemma t n thys

let qed n = 
  let t = Goals.result() 
  in 
  Thydb.add_thm n (Goals.result()) [] (theories()); t

let save_thm ?(simp=false) n th =
  let props = if simp then [Theory.simp_property] else []
  in 
  catch_errors 
    (fun x -> Thydb.add_thm n th props x; th) (theories())

let prove_thm ?(simp=false) n t tacs =
  catch_errors
    (fun x -> 
      let th = Goals.by_list t tacs
      in 
      ignore(save_thm ~simp:simp n th); th)
    ()

let by x = 
  (catch_errors Goals.by_com) x

let scope () = Global.scope();;

let read x= Global.read x
let read_unchecked  x= Global.read_unchecked x
let read_defn  x= Global.read_defn x
