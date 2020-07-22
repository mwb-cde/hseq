(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
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

let catch_errors ppinf f a =
  try f a
  with
    | Report.Error e ->
      Report.print_error ppinf (-1) (Report.Error e);
      raise (Failure "failed")
    | x -> raise x

let save_theory ctxt thy  =
  let fname =
    Filename.concat
    (Context.Files.get_cdir())
    (Context.Files.file_of_thy ctxt (Theory.get_name thy))
  in
  Theory.save_theory thy fname

let load_theory_as_cur ctxt n =
  Context.Files.load_theory_as_cur ctxt n
let read ctxt (x: string) =
  catch_errors (Context.ppinfo ctxt) Context.PP.read ctxt x
let read_unchecked ctxt x =
  catch_errors (Context.ppinfo ctxt) Context.PP.read_unchecked ctxt x
let read_defn ctxt x =
  catch_errors (Context.ppinfo ctxt) Context.PP.read_defn ctxt x

let read_type ctxt (x: string) =
  catch_errors (Context.ppinfo ctxt) Context.PP.read_type ctxt x
let read_type_defn ctxt x =
  catch_errors (Context.ppinfo ctxt) Context.PP.read_type_defn ctxt x

(*
 * Theories
 *)

let set_scope = Context.set_scope
let scope_of = Context.scope_of

let theories ctxt = Context.thydb ctxt

let curr_theory = Context.current
let curr_theory_name  = Context.current_name

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
      try List.append parents [(Context.base_name ctxt)]
      with Not_found -> parents
    in
    let thy = Theory.mk_thy n importing in
    Context.Files.make_current ctxt thy

let end_theory ctxt save =
  if (curr_theory_name ctxt) = ""
  then raise (Report.error "At base theory")
  else
    begin
      let db1 = Thydb.end_current (Context.thydb ctxt) true in
      let ctxt1 = Context.set_thydb ctxt db1 in
      (if save
       then save_theory ctxt1 (Context.current ctxt1)
       else ());
      ctxt1
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
      let db1 = Thydb.end_current (Context.thydb ctxt) false in
      let ctxt1 = Context.set_thydb ctxt db1 in
      save_theory ctxt1 (Context.current ctxt1);
      ctxt1
    end

(*** Theory properties ***)

let parents ctxt ns =
  let thy = Context.current ctxt
  in
  let thy1 = Theory.add_parents ns thy in
  Context.Files.make_current ctxt thy1

let add_file ctxt f =
  let db0 = Context.thydb ctxt in
  let db1 = Thydb.add_file (Context.current_name ctxt) f db0 in
  let ctxt1 = Context.set_thydb ctxt db1
  in
  ctxt1

let remove_file ctxt f =
  let db0 = Context.thydb ctxt in
  let db1 = Thydb.remove_file f (Context.current_name ctxt) db0 in
  Context.set_thydb ctxt db1

(*** Printer and Parser information ***)

(*** Basic PP functions ***)

(*** Types ***)
let add_type_pp_rec ctxt id rcrd =
  let ctxt1 =
    Context.set_thydb
      ctxt
      (Thydb.add_type_pp_rec (Ident.name_of id) rcrd
         (Context.thydb ctxt))
   in
  let ctxt2 = Context.PP.add_type_pp_record ctxt1 id rcrd in
  ctxt2

let remove_type_pp_rec ctxt id =
  let db1 =
    Thydb.remove_type_pp_rec
      (Ident.thy_of id) (Ident.name_of id)
      (Context.thydb ctxt)
  in
  let ctxt1 = Context.set_thydb ctxt db1 in
  Context.PP.remove_type_pp ctxt1 id

let get_type_pp_rec ctxt id = Context.PP.get_type_pp ctxt id

let add_symbol ctxt str sym =
  let ctxt1 =
    Context.set_thydb ctxt
      (Thydb.add_pp_symbol (str, sym) (Context.thydb ctxt))
  in
  Context.PP.add_pp_symbol ctxt1 str sym

(*** Terms ***)

let add_term_pp_rec ctxt id pos rcrd =
  let ppos = Lib.from_option pos Lib.First in
  let ctxt1 =
    Context.set_thydb
      ctxt
      (Thydb.add_term_pp_rec (Ident.name_of id) (rcrd, ppos)
         (Context.thydb ctxt))
  in
  Context.PP.add_term_pp_record ctxt1 id rcrd

let get_term_pp_rec ctxt id = Context.PP.get_term_pp ctxt id

let remove_term_pp_rec ctxt id =
  let db1 =
    Thydb.remove_term_pp_rec
      (Ident.thy_of id) (Ident.name_of id) (theories ctxt)
  in
  let ctxt1 = Context.set_thydb ctxt db1 in
  Context.PP.remove_term_pp ctxt1 id

let add_overload ctxt sym pos id =
  let ppos = Lib.from_option pos Lib.First in
  let ty =
    Thydb.get_id_type (Ident.thy_of id) (Ident.name_of id) (theories ctxt)
  in
  Context.PP.add_overload ctxt sym ppos (id, ty)

let remove_overload ctxt sym id =
  Context.PP.remove_overload ctxt sym id

(*** User-level PP Functions ***)

(*** Types ***)
let add_type_pp ctxt id prec fx repr =
  let rcrd = Printkit.mk_record prec fx repr
  in
  add_type_pp_rec ctxt id rcrd

let remove_type_pp ctxt id = remove_type_pp_rec ctxt id
let get_type_pp id = get_type_pp_rec id

(*** Terms ***)

let add_term_pp ctxt id prec fx repr =
  let rcrd = Printkit.mk_record prec fx repr
  in
  let ctxt1 = add_term_pp_rec ctxt id None rcrd
  in
  begin
    match repr with
      | None -> ctxt1
      | Some(sym) -> add_overload ctxt1 sym None id
  end

let remove_term_pp ctxt id = remove_term_pp_rec ctxt id
let get_term_pp id = get_term_pp_rec id

(** Axioms and Theorems ***)

let defn ctxt id =
  let t, n = Context.PP.read_identifier ctxt id in
  let thys = theories ctxt
  in
  Thydb.get_defn t n thys

let get_theorem ctxt id =
  let t, n = Context.PP.read_identifier ctxt id in
  let thys = theories ctxt
  in
  try Thydb.get_axiom t n thys
  with Not_found -> Thydb.get_theorem t n thys

let thm ctxt id =
  let t, n = Context.PP.read_identifier ctxt id in
  let thys = theories ctxt
  in
  Thydb.get_lemma t n thys

let axiom sctxt n trm =
  let thm = Logic.mk_axiom (Formula.make (Context.scope_of sctxt) trm)
  and props = [] in
  let nctxt =
  begin
    match Lib.try_find (Theory.get_theorem_rec n) (Context.current sctxt) with
      | Some _ ->
        raise (Report.error ("Theorem named "^n^" already exists in theory."))
      | _ ->
        (Context.set_thydb sctxt
           (Thydb.add_axiom n thm props (theories sctxt)))
  end
  in
  (set_scope nctxt (scope_of sctxt), thm)

let prove sctxt trm tac =
  Goals.prove_goal sctxt trm tac

let save_thm ctxt simp n th =
  let props = if simp then [Theory.simp_property] else []
  in
  catch_errors (Context.ppinfo ctxt)
    (fun x ->
      (Context.set_thydb ctxt (Thydb.add_thm n th props x), th))
    (Context.thydb ctxt)

let prove_save_thm ctxt simp n t tacs =
  let prove_aux _ =
    let thm =
      try Goals.by_list ctxt t tacs
      with err ->
        raise (Report.add_error
                 (Report.error ("Failed to prove theorem "^n)) err)
    in
    let (cntxt1, _) = save_thm ctxt simp n thm in
    (cntxt1, thm)
  in
  catch_errors (Context.ppinfo ctxt) prove_aux ()

let theorem ctxt = prove_save_thm ctxt false
let rule ctxt = prove_save_thm ctxt true
let lemma = theorem

let qed ctxt pstk n =
  let thm = Goals.result pstk in
  let thydb = Thydb.add_thm n thm [] (Context.thydb ctxt) in
  let ctxt1 = Context.set_thydb ctxt thydb
  in
  (ctxt1, thm)

let get_or_prove ctxt name trm tac =
  let act () =
    match Lib.try_find (thm ctxt) name with
      | Some x -> x
      | _ ->
        try prove ctxt trm tac
        with err ->
          raise (Report.add_error
                   (Report.error
                      ("get_or_prove: Failed with theorem "^name)) err)
  in
  catch_errors (Context.ppinfo ctxt) act ()

(*
 * Definitions and Declarations
 *)

module Option =
struct

  type t =
    | Symbol of (int * fixity * (string)option) (** The symbol to use *)
    | Simp of bool (** Whether to use as a simplifier rule *)
    | Repr of string  (** The name of a representation function *)
    | Abs of string  (** The name of an abstraction function *)
    | Thm of Logic.thm (** A theorem *)

  type record =
    {
      symbol: (int * fixity * (string)option)option;
      simp: (bool)option;
      repr: (string)option;
      abs: (string)option;
      thm: (Logic.thm)option;
    }

  let default =
    {
      symbol = None;
      simp = None;
      repr = None;
      abs = None;
      thm = None;
    }

  let set rcrd opt =
    match opt with
    | Symbol(p, f, n) -> { rcrd with symbol = Some(p, f, n) }
    | Simp(f) -> { rcrd with simp = Some(f) }
    | Repr(s) -> { rcrd with repr = Some(s) }
    | Abs(s) -> { rcrd with abs = Some(s) }
    | Thm(t) -> { rcrd with thm = Some(t) }

  let rec update rcrd opts =
    match opts with
    | (x::xs) -> update (set rcrd x) xs
    | _ -> rcrd

  let interpret opts = update default opts
end

(*** Type declarations and definitions ***)

(*** Subtype definitions ***)

let subtypedef ctxt (name, args, dtype, set) (rep, abs) ?(simp=true) thm =
  let scp = scope_of ctxt in
  let rep_name = Lib.from_option rep ("REP_"^name)
  and abs_name = Lib.from_option abs ("ABS_"^name)
  in
  let tydef =
    Logic.Defns.mk_subtype scp
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
  let ctxt1 = Context.set_thydb ctxt db3 in
  (* Add the theorems *)
  let rep_type = tyrec.Logic.Defns.rep_type
  and rt_name = rep_name^"_mem"
  and rep_type_inverse = tyrec.Logic.Defns.rep_type_inverse
  and rti_name = rep_name^"_inverse"
  and abs_type_inverse = tyrec.Logic.Defns.abs_type_inverse
  and ati_name = abs_name^"_inverse"
  in
  let (ctxt2, _) = save_thm ctxt1 simp rt_name rep_type in
  let (ctxt3, _) = save_thm ctxt2 simp rti_name rep_type_inverse in
  let (ctxt4, _) = save_thm ctxt3 simp ati_name abs_type_inverse in
  (ctxt4, tydef)

(*** Simple type definitions ***)
let simple_typedef ctxt (n, args, def) =
  let scp = scope_of ctxt in
  let def1 =
    match def with
      | None -> None
      | Some(x) -> Some(Ltype.set_name scp x)
  in
  let tydef = Logic.Defns.mk_typealias scp n args def1 in
  let ctxt1 =
    Context.set_thydb ctxt (Thydb.add_type_rec tydef (theories ctxt))
  in
  (ctxt1, tydef)

(*** Toplevel type definitions and declarations ***)
(*let typedef sctxt ?pp ?simp ?thm ?rep ?abs tydef = *)
let typedef sctxt opts tydef =
  let options = Option.interpret opts in
  let pp = options.symbol
  and simp = options.simp
  and rep = options.repr
  and abs = options.abs
  and thm = options.thm
  in
  let (name, (sctxt1, ret_def)) =
    match tydef with
      | Defn.Parser.NewType (n, args) ->
        (n, simple_typedef sctxt (n, args, None))
      | Defn.Parser.TypeAlias(n, args, d) ->
        (n, simple_typedef sctxt (n, args, Some(d)))
      | Defn.Parser.Subtype(n, args, dtyp, set) ->
        let thm1=
          if thm = None
          then raise (Report.error
                        ("Subtype definition must have an existance theorem"))
          else Lib.from_some thm
        in
        let (ctxt1, def) =
          subtypedef sctxt (n, args, dtyp, set) (rep, abs) ?simp:simp thm1
        in
        (n, (ctxt1, def))
  in
  let sctxt2 =
    if (pp == None) then sctxt1
    else
    begin
      let (prec, fx, repr) = Lib.from_some pp in
      let lname =
        Ident.mk_long (Theory.get_name (Context.current sctxt1)) name
      in
      add_type_pp sctxt1 lname prec fx repr
    end
  in
  (sctxt2, ret_def)

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

let define ctxt opts (((name, nty), args), rhs) =
  let options = Option.interpret opts in
  let pp = options.symbol
  and simp = Lib.from_option options.simp false
  in
  let scp = scope_of ctxt in
  let new_def =
    let curr_thy_name =
      Theory.get_name (Context.current ctxt) in
    Logic.Defns.mk_termdef scp
      (Ident.mk_long curr_thy_name name, nty)
      args rhs
  in
  let props = if simp then [Theory.simp_property] else [] in
  let (n, ty, d) = Logic.Defns.dest_termdef new_def
  in
  let ctxt1 =
    Context.set_thydb ctxt
      (Thydb.add_defn (Ident.name_of n) ty d props (theories ctxt))
  in
  let ctxt2 =
    begin
      match pp with
        | None -> ctxt1
        | Some(prec, fx, repr) -> add_term_pp ctxt1 n prec fx repr
    end
  in
  (ctxt2, new_def)

(*** Term declarations ***)

let declare ctxt opts trm =
  let options = Option.interpret opts in
  let pp = options.symbol
  in
  let (ctxt2, val_name, val_type) =
    let declare_aux () =
      let (v, ty) =
        match (Term.dest_atom trm) with
        | Term.Free(i, t) -> (i, t)
        | Term.Id(i, t) -> (Ident.name_of i, t)
        | _ ->
           raise (Report.error "expected an identifier or a free variable")
      in
      let id = Ident.mk_long (Context.current_name ctxt) v in
      let dcl = Logic.Defns.mk_termdecln (scope_of ctxt) v ty in
      let ctxt1 =
        Context.set_thydb ctxt
                          (Thydb.add_decln dcl [] (theories ctxt))
      in
      (ctxt1, id, ty)
    in
    begin
      try declare_aux()
      with err ->
        raise (Report.add_error
                 (Term.term_error "Badly formed declaration" [trm])
                 err)
    end
  in
  begin
    match pp with
      | None -> (ctxt2, val_name, val_type)
      | Some(prec, fx, repr) ->
        let longname =
          if (Ident.thy_of val_name) = Ident.null_thy
          then
            Ident.mk_long (Context.current_name ctxt2)
              (Ident.name_of val_name)
          else val_name
        in
        let ctxt3 = add_term_pp ctxt2 longname prec fx repr
        in
        (ctxt3, val_name, val_type)
  end
