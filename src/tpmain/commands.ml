(*----
 Name: commands.ml
 Copyright M Wahab 2005-2010
 Author: M Wahab  <mwb.cde@googlemail.com>

 This file is part of HSeq

 HSeq is free software; you can redistribute it and/or modify it under
 the terms of the Lesser GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 HSeq is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
 License for more details.

 You should have received a copy of the Lesser GNU General Public
 License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
----*)

open Report

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
    Report.Error e -> 
      Report.print_error (Global.PP.info()) (-1) (Report.Error e); 
      raise (Failure "failed")
  | x -> raise x)

let save_theory thy prot= 
  let fname = Filename.concat
      (Global.Files.get_cdir()) 
      (Global.Files.file_of_thy (Theory.get_name thy))
  in 
  Theory.save_theory thy fname

let load_theory_as_cur n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in if t=n then n else chop t
  in 
(*
  let filefn fname = Global.Files.find_thy_file fname
  in 
*)
  let db = 
    Thydb.Loader.load (Global.theories()) Global.Files.loader_data
      (Thydb.Loader.mk_info n None None)
  in 
  Global.Thys.set_theories(db)

let read x = catch_errors Global.read x
let read_unchecked  x= catch_errors Global.PP.read_unchecked x
let read_defn  x= catch_errors Global.read_defn x

(*** 
* Theories
***)

let scope () = Global.scope()
let theories () = Global.theories()

let curr_theory () = Global.current()
let curr_theory_name () = Global.current_name()

let theory name = 
  if name = "" 
  then curr_theory()
  else Thydb.get_thy (theories()) name

(*** Begining and Ending theories ***)
 
let begin_theory n parents= 
  if n = "" 
  then (raise (Report.error "No theory name"))
  else 
    let importing=
      try
	List.append parents [(Global.Thys.get_base_name())]
      with Not_found -> parents
    in 
    let db = theories()
    and thy = Theory.mk_thy n importing
    in
    let db1 = Thydb.Loader.make_current db Global.Files.loader_data thy
    in 
    Global.Thys.set_theories(db1)

let end_theory ?(save=true) () = 
  if curr_theory_name() = "" 
  then (raise (Report.error "At base theory"))
  else 
    (let thy = curr_theory()
    in 
    Theory.end_theory thy true;
    if(save) then save_theory thy true else ())


let open_theory n =
  if n = "" 
  then (raise (Report.error "No theory name"))
  else (load_theory_as_cur n)

let close_theory () = 
  if curr_theory_name() = "" 
  then (raise (Report.error "At base theory"))
  else 
    (let thy = curr_theory()
    in 
    Theory.end_theory thy false;
    save_theory thy false)

(*** Theory properties ***)

let parents ns = 
  let thy = curr_theory()
  and db = theories()
  in 
  Theory.add_parents ns thy;
  let db1 = 
    Thydb.Loader.make_current db Global.Files.loader_data thy
  in 
  Global.Thys.set_theories db1

let add_file ?(use=false) f =
  Theory.add_file f (curr_theory());
  if use
  then 
    Global.Files.load_use_file f
  else ()
      
let remove_file f =
  Theory.remove_file f (curr_theory())

(*** Printer and Parser information ***)

(*** Basic PP functions ***)

(*** Types ***)
let add_type_pp_rec id rcrd=
  Global.Thys.set_theories
    (Thydb.add_type_pp_rec (Ident.name_of id) rcrd (theories()));
  Global.PP.add_type_pp_record id rcrd
      
let remove_type_pp_rec id =
  Thydb.remove_type_pp_rec
    (Ident.thy_of id) (Ident.name_of id) (theories());
  Global.PP.remove_type_pp id

let get_type_pp_rec id= Global.PP.get_type_pp id 

(*** Terms ***)

let add_term_pp_rec id ?(pos=Lib.First) rcrd=
  Global.Thys.set_theories
    (Thydb.add_term_pp_rec (Ident.name_of id) (rcrd, pos) (theories()));
  Global.PP.add_term_pp_record id rcrd
      
let get_term_pp_rec id= Global.PP.get_type_pp id 

let remove_term_pp_rec id =
  Thydb.remove_term_pp_rec
       (Ident.thy_of id) (Ident.name_of id) (theories());
  Global.PP.remove_term_pp id

let add_overload sym ?(pos=Lib.First) id = 
  let ty = 
    Thydb.get_id_type (Ident.thy_of id) (Ident.name_of id) (theories())
  in 
  Parser.add_overload sym pos (id, ty)

let remove_overload sym id =
  Parser.remove_overload sym id

(*** User-level PP Functions ***)

(*** Types ***)
let add_type_pp id prec fx repr=
  let rcrd=Printer.mk_record prec fx repr
  in 
  add_type_pp_rec id rcrd
let remove_type_pp id = remove_type_pp_rec id
let get_type_pp id=get_type_pp_rec id

(*** Terms ***)

let add_term_pp id ?(pos=Lib.First) prec fx repr=
  let rcrd=Printer.mk_record prec fx repr
  in 
  add_term_pp_rec id ~pos:pos rcrd;
  match repr with
    None -> () (* add_overload (Ident.name_of id) ~pos:pos id *)
  | Some(sym) -> add_overload sym ~pos:pos id

let remove_term_pp id = remove_term_pp_rec id
let get_term_pp id=get_term_pp_rec id


(** Axioms and Theorems ***)

let defn id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in Thydb.get_defn t n thys

let get_theorem id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in 
  try
    Thydb.get_axiom t n thys
  with Not_found -> Thydb.get_theorem t n thys

let thm id =
  let t, n = Global.read_identifier id
  in 
  let thys=theories()
  in 
  Thydb.get_lemma t n thys


let axiom ?(simp=false) n trm =
  let thm = Logic.mk_axiom (Formula.make (Global.scope()) trm)
  and props = if simp then [Theory.simp_property] else []
  in 
  match Lib.try_find (Theory.get_theorem_rec n) (curr_theory()) with
    None -> 
      Global.Thys.set_theories(Thydb.add_axiom n thm props (theories())); 
      thm
  | _ -> 
      raise (Report.error ("Theorem named "^n^" already exists in theory."))

let prove ?scp trm tac = 
  let uscp = 
    match scp with
      None -> scope()
    | Some x -> x
  in
  Goals.prove_goal uscp trm tac

let save_thm ?(simp=false) n th =
  let props = if simp then [Theory.simp_property] else []
  in 
  catch_errors 
    (fun x -> Global.Thys.set_theories(Thydb.add_thm n th props x); th) 
    (theories())

let prove_thm ?(simp=false) n t tacs =
  catch_errors
    (fun x -> 
       let th = 
	 try 
	   Goals.by_list t tacs
	 with 
	     err -> 
	       raise 
		 (Report.add_error
		 (Report.error ("Failed to prove theorem "^n)) err)
       in 
	 ignore(save_thm ~simp:simp n th); th)
    ()

let theorem = prove_thm
let lemma = theorem

let qed n = 
  let thm = Goals.result() 
  in 
  Global.Thys.set_theories(Thydb.add_thm n (Goals.result()) [] (theories())); 
  thm

let get_or_prove name trm tac = 
  let act () =
    match (Lib.try_find thm name) with
      Some x -> x
    | _ -> 
	try (prove trm tac) 
	with 
	  err -> 
	    raise 
	      (Report.add_error 
		 (Report.error 
		    ("get_or_prove: Failed with theorem "^name)) err)
  in 
  catch_errors act ()

(*** 
* Definitions and Declarations 
***)

(*** Type declarations and definitions ***)

(*** Subtype definitions ***)

let subtypedef (name, args, dtype, set) (rep, abs) ?(simp=true) thm=
  let rep_name = Lib.get_option rep ("REP_"^name)
  and abs_name = Lib.get_option abs ("ABS_"^name)
  in 
  let tydef = 
    Logic.Defns.mk_subtype (Global.scope()) 
      name args dtype set rep_name abs_name thm
  in 
  (* Extract the type definition and the declarations of rep and abs *)
  let tyrec = Logic.Defns.dest_subtype tydef
  in 
  let rep_decln = tyrec.Logic.Defns.type_rep
  and abs_decln = tyrec.Logic.Defns.type_abs
  in 
  (* 
     Add the type definition and the declarations of rep and abs to the
     global database
   *)
  let db = theories()
  in 
  let db1 = Thydb.add_type_rec tydef db
  in 
  let db2= Thydb.add_decln rep_decln [] db1
  in 
  let db3 = Thydb.add_decln abs_decln [] db2
  in 
  Global.Thys.set_theories(db3);
  (* Add the theorems *)
  let rep_type = tyrec.Logic.Defns.rep_type
  and rt_name = rep_name^"_mem"
  and rep_type_inverse = tyrec.Logic.Defns.rep_type_inverse
  and rti_name = rep_name^"_inverse"
  and abs_type_inverse = tyrec.Logic.Defns.abs_type_inverse
  and ati_name = abs_name^"_inverse"
  in 
  ignore(save_thm ~simp:simp rt_name rep_type);
  ignore(save_thm ~simp:simp rti_name rep_type_inverse);
  ignore(save_thm ~simp:simp ati_name abs_type_inverse);
  tydef
    
(*** Simple type definitions ***)
let simple_typedef (n, args, def) =
  let def1 = 
    match def with 
      None -> None
    | Some(x) -> Some(Gtypes.set_name (Global.scope()) x)
  in 
  let tydef = Logic.Defns.mk_typealias (Global.scope()) n args def1
  in 
  Global.Thys.set_theories(Thydb.add_type_rec tydef (theories())); tydef


(*** Toplevel type definitions and declarations ***)
let typedef ?pp ?simp ?thm ?rep ?abs tydef = 
  let (name, td) =
    match tydef with
      Defn.Parser.NewType (n, args) -> 
	(n, simple_typedef (n, args, None))
    | Defn.Parser.TypeAlias(n, args, d) -> 
	(n, simple_typedef(n, args, Some(d)))
    | Defn.Parser.Subtype(n, args, dtyp, set) -> 
	let thm1=
	  Lib.dest_option 
	    ~err:(Report.error 
		    ("Subtype definition must have an existance theorem"))
	    thm
	in 
	(n, subtypedef (n, args, dtyp, set) (rep, abs) ?simp:simp thm1)
  in 
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> 
      let lname = Ident.mk_long (Theory.get_name (Global.current())) name
      in 
      add_type_pp lname prec fx repr);
  td


(*** Term declerations and definitions ***)


(**
   [dest_defn_term trm]

   for a term [trm] of the form [f a1 a2 ... an = r] where [n>=0]
   return (f, [a1; a2; ...; an], r)

   for all other terms, raise Failure.
 *)
let dest_defn_term trm=
  let err()= failwith "Badly formed definition"
  in 
  if Lterm.is_equality trm 
  then
    let (lhs, rhs)=Lterm.dest_equality trm
    in 
    let (f, args) =
      if(Term.is_fun lhs)
      then Term.dest_fun lhs
      else 
	if(Term.is_ident lhs)
	then (Term.get_ident_id lhs, [])
	else err()
    in 
    let rargs=List.map Term.dest_ident args
    in
    (Ident.name_of f, 
     (List.map (fun (x, y) -> (Ident.name_of x), y) rargs), rhs)
  else err()
      
(*** Term definitions ***)

let define ?pp ?(simp=false) (((name, nty), args), r)=
  let scp = Global.scope()
  in 
  let ndef=
    Logic.Defns.mk_termdef scp
      (Ident.mk_long (Theory.get_name (Global.current())) name, 
       nty)
      args r
  in 
  let props = 
    if simp
    then [Theory.simp_property]
    else []
  in 
  let (n, ty, d)= Logic.Defns.dest_termdef ndef
  in 
  Global.Thys.set_theories
    (Thydb.add_defn (Ident.name_of n) ty d props (theories())); 
  (match pp with 
    None -> ()
  | Some(prec, fx, repr) -> add_term_pp n prec fx repr);
  ndef

(*** Term declarations ***)

let declare ?pp trm = 
  let n, ty=
    try 
      let v, ty=
	match trm with
	  Basic.Free(i, t) -> (i, t)
	| Basic.Id(i, t) -> (Ident.name_of i, t)
	| _ -> raise (Failure "Badly formed declaration")
      in 
      let id = Ident.mk_long (Global.current_name()) v
      in 
      let dcl = Logic.Defns.mk_termdecln (Global.scope()) v ty
      in 
      Global.Thys.set_theories(Thydb.add_decln dcl [] (theories())); 
      (id, ty)
    with _ -> raise (Report.error ("Badly formed declaration"))
  in 
  match pp with 
    None -> (n, ty)
  | Some(prec, fx, repr) ->
      let longname = 
	if (Ident.thy_of n) = Ident.null_thy 
	then 
	  (Ident.mk_long (Global.current_name()) (Ident.name_of n))
	else n
      in 
      add_term_pp longname prec fx repr;
      (n, ty)

