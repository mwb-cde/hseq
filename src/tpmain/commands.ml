(*----
  Name: commands.ml
  Copyright M Wahab 2005-2010, 2012
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
let nonfix = Parserkit.Info.nonfix 
let prefix = Parserkit.Info.prefix
let suffix = Parserkit.Info.suffix
let infixl = Parserkit.Info.infix Parserkit.Info.left_assoc
let infixr = Parserkit.Info.infix Parserkit.Info.right_assoc
let infixn = Parserkit.Info.infix Parserkit.Info.non_assoc

let catch_errors f a =
  try f a 
  with 
    | Report.Error e -> 
      Report.print_error (Global.PP.info()) (-1) (Report.Error e); 
      raise (Failure "failed")
    | x -> raise x

let save_theory ctxt thy prot = 
  let fname = 
    Filename.concat
    (Context.Files.get_cdir()) 
    (Context.Files.file_of_thy ctxt (Theory.get_name thy))
  in 
  Theory.save_theory thy fname

let load_theory_as_cur ctxt n = 
  let rec chop n = 
    let t = try (Filename.chop_extension n) with _ -> n
    in
    if t = n then n else chop t
  in 
  let db = 
    Thydb.Loader.load (Context.thydb ctxt) (Context.loader_data ctxt)
      (Thydb.Loader.mk_info n None None)
  in 
  Context.Thys.set_theories db ctxt

let read x = catch_errors Global.read x
let read_unchecked  x= catch_errors Global.PP.read_unchecked x
let read_defn  x= catch_errors Global.read_defn x

(*
 * Theories
 *)

let scoped = Context.scoped
let scope_of = Context.scope_of 
let context_of = Context.context_of 
let set_scope = Context.set_scope
let set_context = Context.set_context

let theories ctxt = Context.Thys.theories ctxt

let curr_theory = Context.Thys.current
let curr_theory_name  = Context.Thys.current_name

let theory ctxt name = 
  if name = "" 
  then curr_theory ctxt
  else Thydb.get_thy (theories ctxt) name

(*** Begining and Ending theories ***)
    
let begin_theory ctxt n parents = 
  if n = "" 
  then raise (Report.error "No theory name")
  else 
    let importing =
      try List.append parents [(Context.Thys.get_base_name ctxt)]
      with Not_found -> parents
    in 
    let db = Context.Thys.theories ctxt
    and thy = Theory.mk_thy n importing
    in
    let db1 = Thydb.Loader.make_current db (Context.loader_data ctxt) thy
    in 
    Context.Thys.set_theories db1 ctxt

let end_theory ctxt ?(save=true) () = 
  if (curr_theory_name ctxt) = "" 
  then raise (Report.error "At base theory")
  else 
    begin
      let thy = curr_theory ctxt
      in 
      Theory.end_theory thy true;
      if save then save_theory ctxt thy true else ();
      ctxt
    end

let open_theory ctxt n =
  if n = "" 
  then raise (Report.error "No theory name")
  else load_theory_as_cur ctxt n

let close_theory ctxt = 
  if (curr_theory_name ctxt) = "" 
  then raise (Report.error "At base theory")
  else 
    begin
      let thy = curr_theory ctxt
      in 
      Theory.end_theory thy false;
      save_theory ctxt thy false
    end

(*** Theory properties ***)

let parents ctxt ns = 
  let thy = Context.Thys.curr_theory ctxt
  and db = Context.Thys.theories ctxt
  in 
  Theory.add_parents ns thy;
  let db1 = Thydb.Loader.make_current db (Context.loader_data ctxt) thy
  in 
  Context.Thys.set_theories db1 ctxt

let add_file ctxt ?(use=false) f =
  Theory.add_file f (Context.Thys.curr_theory ctxt);
  if use
  then Context.Files.load_use_file ctxt f
  else ()
    
let remove_file ctxt f = Theory.remove_file f (curr_theory ctxt)

(*** Printer and Parser information ***)

(*** Basic PP functions ***)

(*** Types ***)
let add_type_pp_rec ctxt id rcrd =
  let ctxt1 =
    Context.Thys.set_theories 
      (Thydb.add_type_pp_rec (Ident.name_of id) rcrd (Context.Thys.theories ctxt))
      ctxt
  in
  Global.PP.add_type_pp_record id rcrd;
  ctxt1
    
let remove_type_pp_rec ctxt id =
  Thydb.remove_type_pp_rec 
    (Ident.thy_of id) (Ident.name_of id) (Context.Thys.theories ctxt);
  Global.PP.remove_type_pp id;
  ctxt

let get_type_pp_rec id = Global.PP.get_type_pp id 

(*** Terms ***)

let add_term_pp_rec ctxt id ?(pos=Lib.First) rcrd =
  let ctxt1 = 
    Context.Thys.set_theories
      (Thydb.add_term_pp_rec (Ident.name_of id) (rcrd, pos) (Context.Thys.theories ctxt))
      ctxt
  in
  Global.PP.add_term_pp_record id rcrd; ctxt1
    
let get_term_pp_rec id = Global.PP.get_type_pp id 

let remove_term_pp_rec ctxt id =
  Thydb.remove_term_pp_rec
    (Ident.thy_of id) (Ident.name_of id) (theories ctxt);
  Global.PP.remove_term_pp id;
  ctxt

let add_overload ctxt sym ?(pos=Lib.First) id = 
  let ty = 
    Thydb.get_id_type (Ident.thy_of id) (Ident.name_of id) (theories ctxt)
  in 
  Parser.add_overload sym pos (id, ty)

let remove_overload sym id =
  Parser.remove_overload sym id

(*** User-level PP Functions ***)

(*** Types ***)
let add_type_pp ctxt id prec fx repr =
  let rcrd = Printer.mk_record prec fx repr
  in 
  add_type_pp_rec ctxt id rcrd

let remove_type_pp ctxt id = ignore(remove_type_pp_rec ctxt id)
let get_type_pp id = get_type_pp_rec id

(*** Terms ***)

let add_term_pp ctxt id ?(pos=Lib.First) prec fx repr =
  let rcrd = Printer.mk_record prec fx repr
  in 
  let ctxt1 = add_term_pp_rec ctxt id ~pos:pos rcrd
  in
  begin
    match repr with
      | None -> ()
      | Some(sym) -> add_overload ctxt1 sym ~pos:pos id
  end;
  ctxt1

let remove_term_pp ctxt id = ignore(remove_term_pp_rec ctxt id)
let get_term_pp id = get_term_pp_rec id

(** Axioms and Theorems ***)

let defn ctxt id =
  let t, n = Global.read_identifier id in 
  let thys = theories ctxt
  in
  Thydb.get_defn t n thys

let get_theorem ctxt id =
  let t, n = Global.read_identifier id in 
  let thys = theories ctxt
  in 
  try Thydb.get_axiom t n thys
  with Not_found -> Thydb.get_theorem t n thys

let thm ctxt id =
  let t, n = Global.read_identifier id in 
  let thys = theories ctxt
  in 
  Thydb.get_lemma t n thys

let axiom ctxt ?(simp=false) n trm =
  let thm = Logic.mk_axiom (Formula.make (Global.scope()) trm)
  and props = if simp then [Theory.simp_property] else []
  in 
  begin
    match Lib.try_find (Theory.get_theorem_rec n) (curr_theory ctxt) with
      | Some _ -> 
        raise (Report.error ("Theorem named "^n^" already exists in theory."))
      | _ -> 
        (Context.Thys.set_theories (Thydb.add_axiom n thm props (theories ctxt)) ctxt,
         thm)
  end

let prove sctxt trm tac = 
  Goals.prove_goal (Context.scope_of sctxt) trm tac

let save_thm ctxt ?(simp=false) n th =
  let props = if simp then [Theory.simp_property] else []
  in 
  catch_errors 
    (fun x -> (Context.Thys.set_theories(Thydb.add_thm n th props x) ctxt, th)) 
    (Context.Thys.theories ctxt)

let prove_thm ctxt  ?(simp=false) n t tacs =
  let prove_aux _ = 
    let thm = 
      try Goals.by_list t tacs
      with err -> 
        raise (Report.add_error
		 (Report.error ("Failed to prove theorem "^n)) err)
    in 
    let (cntxt1, _) = save_thm ctxt ~simp:simp n thm in
    (cntxt1, thm)
  in 
  catch_errors prove_aux ()

let theorem = prove_thm
let lemma = theorem

let qed n = 
  let thm = Goals.result() in  
  let ctxt = Global.state() in
  let thydb = Thydb.add_thm n (Goals.result()) [] (Context.thydb ctxt) in
  let ctxt1 = Context.set_thydb thydb ctxt 
  in
  Global.set_state ctxt1; thm

let get_or_prove sctxt name trm tac = 
  let act () =
    match Lib.try_find (thm (context_of sctxt)) name with
      | Some x -> x
      | _ -> 
	try prove sctxt trm tac
	with err -> 
	  raise (Report.add_error 
	           (Report.error 
		      ("get_or_prove: Failed with theorem "^name)) err)
  in 
  catch_errors act ()

(*
 * Definitions and Declarations 
 *)

(*** Type declarations and definitions ***)

(*** Subtype definitions ***)

let subtypedef sctxt (name, args, dtype, set) (rep, abs) ?(simp=true) thm =
  let ctxt = context_of sctxt in
  let rep_name = Lib.get_option rep ("REP_"^name)
  and abs_name = Lib.get_option abs ("ABS_"^name)
  in 
  let tydef = 
    Logic.Defns.mk_subtype (Global.Old.scope()) 
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
  let db = theories ctxt in 
  let db1 = Thydb.add_type_rec tydef db in 
  let db2 = Thydb.add_decln rep_decln [] db1 in 
  let db3 = Thydb.add_decln abs_decln [] db2
  in 
  let ctxt1 = Context.Thys.set_theories db3 ctxt in
  (* Add the theorems *)
  let rep_type = tyrec.Logic.Defns.rep_type
  and rt_name = rep_name^"_mem"
  and rep_type_inverse = tyrec.Logic.Defns.rep_type_inverse
  and rti_name = rep_name^"_inverse"
  and abs_type_inverse = tyrec.Logic.Defns.abs_type_inverse
  and ati_name = abs_name^"_inverse"
  in 
  let (ctxt2, _) = save_thm ctxt1 ~simp:simp rt_name rep_type in
  let (ctxt3, _) = save_thm ctxt2 ~simp:simp rti_name rep_type_inverse in
  let (ctxt4, _) = save_thm ctxt3 ~simp:simp ati_name abs_type_inverse in
  (set_context sctxt ctxt4, tydef)
    
(*** Simple type definitions ***)
let simple_typedef sctxt (n, args, def) =
  let scp = scope_of sctxt 
  and ctxt = context_of sctxt in 
  let def1 = 
    match def with 
      | None -> None
      | Some(x) -> Some(Gtypes.set_name scp x)
  in 
  let tydef = Logic.Defns.mk_typealias scp n args def1 in 
  let ctxt1 =
    Context.Thys.set_theories (Thydb.add_type_rec tydef (theories ctxt)) ctxt
  in
  (set_context sctxt ctxt1, tydef)

(*** Toplevel type definitions and declarations ***)
let typedef (sctxt: Context.scoped) ?pp ?simp ?thm ?rep ?abs tydef = 
  let (name, (sctxt1, ret_def)) =
    match tydef with
      | Defn.Parser.NewType (n, args) -> 
	(n, simple_typedef sctxt (n, args, None))
      | Defn.Parser.TypeAlias(n, args, d) -> 
	(n, simple_typedef sctxt (n, args, Some(d)))
      | Defn.Parser.Subtype(n, args, dtyp, set) -> 
	let thm1=
	  Lib.dest_option 
	    ~err:(Report.error 
                    ("Subtype definition must have an existance theorem"))
	    thm
        in 
        let (ctxt1, def) = 
          subtypedef sctxt 
            (n, args, dtyp, set) (rep, abs) ?simp:simp thm1
        in 
	(n, (ctxt1, def))
  in 
  let ctxt2 = 
    let ctxt1 = context_of sctxt1 in
    begin
      match pp with 
        | None -> ctxt1
        | Some(prec, fx, repr) -> 
          let lname = 
            Ident.mk_long (Theory.get_name (Context.Thys.current ctxt1)) name
          in 
          add_type_pp ctxt1 lname prec fx repr
    end
  in
  (set_context sctxt ctxt2, ret_def)

(*** Term declerations and definitions ***)

(**
   [dest_defn_term trm]

   For a term [trm] of the form [f a1 a2 ... an = r] where [n>=0]
   return [(f, [a1; a2; ...; an], r)].

   For all other terms, raise Failure.
*)
let dest_defn_term trm =
  let err() = failwith "Badly formed definition"
  in 
  if Lterm.is_equality trm 
  then
    let (lhs, rhs) = Lterm.dest_equality trm in 
    let (f, args) =
      if Term.is_fun lhs
      then Term.dest_fun lhs
      else 
	if Term.is_ident lhs
	then (Term.get_ident_id lhs, [])
	else err()
    in 
    let rargs = List.map Term.dest_ident args
    in
    (Ident.name_of f, 
     List.map (fun (x, y) -> (Ident.name_of x), y) rargs, 
     rhs)
  else err()
    
(*** Term definitions ***)

let define sctxt ?pp ?(simp=false) (((name, nty), args), rhs) =
  let scp = scope_of sctxt 
  and ctxt = context_of sctxt in 
  let new_def =
    let curr_thy_name = 
      Theory.get_name (Context.Thys.current ctxt) in
    Logic.Defns.mk_termdef scp
      (Ident.mk_long curr_thy_name name, nty)
      args rhs
  in 
  let props = if simp then [Theory.simp_property] else [] in 
  let (n, ty, d) = Logic.Defns.dest_termdef new_def
  in 
  let ctxt1 = 
    Context.Thys.set_theories
      (Thydb.add_defn (Ident.name_of n) ty d props (theories ctxt)) ctxt 
  in
  let ctxt2 = 
    begin
      match pp with 
        | None -> ctxt1
        | Some(prec, fx, repr) -> add_term_pp ctxt1 n prec fx repr
    end 
  in
  (set_context sctxt ctxt2, new_def)

(*** Term declarations ***)

let declare sctxt ?pp trm = 
  let ctxt = context_of sctxt in 
  let (ctxt2, val_name, val_type) =
    let declare_aux () = 
      let v, ty =
	match trm with
	  | Basic.Free(i, t) -> (i, t)
	  | Basic.Id(i, t) -> (Ident.name_of i, t)
	  | _ -> raise (Failure "Badly formed declaration")
      in 
      let id = Ident.mk_long (Context.Thys.current_name ctxt) v in 
      let dcl = Logic.Defns.mk_termdecln (scope_of sctxt) v ty in 
      let ctxt1 = 
        Context.Thys.set_theories(Thydb.add_decln dcl [] (theories ctxt)) ctxt
      in
      (ctxt1, id, ty)
    in
    try declare_aux ()
    with _ -> raise (Report.error ("Badly formed declaration"))
  in 
  begin
    match pp with 
      | None -> (set_context sctxt ctxt2, val_name, val_type)
      | Some(prec, fx, repr) ->
        let longname = 
	  if (Ident.thy_of val_name) = Ident.null_thy 
	  then 
            Ident.mk_long (Context.Thys.current_name ctxt2) 
              (Ident.name_of val_name)
	  else val_name
        in 
        let ctxt3 = add_term_pp ctxt2 longname prec fx repr 
        in
        (set_context sctxt ctxt3, val_name, val_type)
  end
