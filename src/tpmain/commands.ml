open Result

let catch_errors f a =
  (try f a 
  with 
    Result.Error e -> 
      Result.print_error (Tpenv.base_pp_state()) (-1) (Result.Error e); 
      raise(Result.Error e)
  | x -> raise x)

let theories () = Tpenv.get_theories()

let read x = catch_errors Tpenv.read x

let curr_theory () = (Tpenv.get_cur_thy())
let get_theory_name thy = (curr_theory()).Theory.name

let save_theory thy prot= 
  if not (Theory.get_protection thy)
  then 
    (let fname = Filename.concat
	(Tpenv.get_cdir()) ((get_theory_name thy)^".thy")
    in let oc = open_out fname
    in 
    Theory.export_theory oc thy prot;
    close_out oc)
  else raiseError ("Theory "^(Theory.get_name thy)^" is protected")

      (*if prot then (Theory.set_protection thy) else ();
	 Theory.output_theory oc thy; *)

let load_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Tpenv.find_thy_file fname
  in 
  ignore(Thydb.load_theory (theories()) n false Tpenv.on_load_thy filefn)

let load_parent_theory n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Tpenv.find_thy_file fname
  in 
  ignore(Thydb.load_theory (theories()) n true Tpenv.on_load_thy filefn)

let load_theory_as_cur n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in let filefn fname = Tpenv.find_thy_file fname
  in let imprts=
    (Thydb.load_theory (theories()) n false Tpenv.on_load_thy filefn)
  in 
  (Tpenv.set_cur_thy (Thydb.getthy (theories()) n);
   Thydb.add_importing imprts (theories()))

let new_theory n = 
  if n = "" then (raiseError "No theory name")
  else Tpenv.set_cur_thy (Theory.mk_thy n)

let open_theory n =
  if n = "" 
  then (raiseError "No theory name")
  else (load_theory_as_cur n)

let close_theory() = 
  if get_theory_name() = "" 
  then (raiseError "At base theory")
  else save_theory (curr_theory()) false

let end_theory() = 
  if get_theory_name() = "" 
  then (raiseError "At base theory")
  else save_theory (curr_theory()) true

(*
   let mk_typedef_rec n ags d cs =
   {Gtypes.name = n;
   Gtypes.args = ags;
   Gtypes.alias = d;
   Gtypes.characteristics = cs}

   let new_type st = 
   let (n, args, def)= Tpenv.read_type_defn st
   in 
   let trec = mk_typedef_rec n args def []
   in Thydb.add_type_rec n trec (theories())
 *)

let new_type st = 
  let (n, args, def)= Tpenv.read_type_defn st
  in 
  let trec = Logic.Defns.mk_typedef (Tpenv.typenv()) n args def 
  in Thydb.add_type_rec trec (theories())

let new_defn str =
  let ((name,args), r) = Tpenv.read_defn str
  in 
  let (n, ty, d) = Defn.mkdefn (Tpenv.typenv()) name args r
  in 
  Theory.add_defn n ty d (curr_theory()); 
  (n, ty, d)

let define str = 
  let ((name, args), r)=Tpenv.read_defn str
  in 
  let (n, ty, d)= Defn.mkdefn (Tpenv.typenv()) name args r
  in (Thydb.add_defn n ty d (theories());
      (n, ty, d))

let new_infix_defn str priority=
  let ((name, args), r) = Tpenv.read_defn str
  in let (n, ty, d) = Defn.mkdefn (Tpenv.typenv()) name args r
  in 
  Thydb.add_defn_rec n ty (Some d) true priority 
    (theories());

  Tpenv.add_id_info 
    (Basic.mklong (Tpenv.get_cur_name()) n)
    (Corepp.mk_pp_rec priority true None);
  (n, ty, d)

let new_decl sl sty =
  let l =  Tpenv.read_identifier sl
  and ty = Tpenv.read_fulltype sty
  in 
  let dcl = (Defn.mkdecln (Tpenv.typenv()) l ty)
  in Thydb.add_decln_rec dcl 0 (theories())

let declare str =
  let t = Tpenv.read_unchecked str
  in 
  try
    (let (v, ty)=Term.dest_typed t
    in let (n, _) = Term.dest_var v
    in let dcl= (Defn.mkdecln (Tpenv.typenv()) n ty)
    in 
    Thydb.add_decln_rec dcl 0 (theories()))
  with _ -> (raiseError "Badly formed declaration")

let new_full_defn str infx prec trans =
  let ((name, args), r) = Tpenv.read_defn str
  in 
  let (n, ty, d) = Defn.mkdefn (Tpenv.typenv()) name args r
  in 
  Thydb.add_defn_rec n ty (Some d)
    infx prec (theories());

  if infx then
    let pprc = (Corepp.mk_pp_rec prec infx (Some trans))
    in 
    Thydb.add_pp_rec (Basic.fn_id) n pprc (theories());
    Tpenv.add_id_info 
      (Basic.mklong (Tpenv.get_cur_name()) n) pprc
  else ();
  (n, ty, d)

let new_full_decln sl sty inf prec trans=
  let l = Tpenv.read_identifier sl
  and ty = Tpenv.read_fulltype sty
  in 
  let n, ty = Defn.dest_decln (Defn.mkdecln (Tpenv.typenv()) l ty)
  in Thydb.add_defn_rec 
    (Basic.name l) ty None inf prec (theories());
  if inf then
    let pprc = (Corepp.mk_pp_rec prec inf (Some trans))
    and longname = 
      if (Basic.thy_of_id n) = Basic.null_thy 
      then 
	(Basic.mklong (Tpenv.get_cur_name()) (Basic.name n))
      else n
    in 
    Thydb.add_pp_rec (Basic.fn_id) (Basic.name n) pprc (theories());
    Tpenv.add_id_info longname  pprc
  else ();
  (n, ty)

let new_axiom n str =
  let t = Logic.mk_axiom 
      (Formula.form_of_term (Tpenv.typenv()) (Tpenv.read str))
  in Thydb.add_axiom n t (theories()); t

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

let parents ns = 
  List.iter load_parent_theory ns;
  Theory.add_parents ns (curr_theory());
  Thydb.add_importing (Thydb.mk_importing (theories())) (theories())

let qed n = 
  let t = Goals.result() 
  in 
  Thydb.add_thm n (Goals.result()) (theories()); t

(*
   let prove_theorem n t tacs =
   catch_errors
   (fun x -> 
   let nt = Goals.prove_goal t ((Tactics.apply_list tacs))
   in 
   (Thydb.add_thm n nt x); nt)
   (theories())
 *)
let prove_theorem n t tacs =
  catch_errors
    (fun x -> 
      let nt = Goals.by_list t tacs
      in 
      (Thydb.add_thm n nt x); nt)
    (theories())


let save_theorem n th =
  catch_errors 
    (fun x -> Thydb.add_thm n th x; th) (theories())
    

let by x = 
  (catch_errors Goals.by_com) x


let scope () = Tpenv.typenv();;
