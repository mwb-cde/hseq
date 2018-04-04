(*----
  Name: theory.ml
  Copyright Matthew Wahab 2005-2016
  Author: Matthew Wahab <mwb.cde@gmail.com>

  This file is part of HSeq

  HSeq is free software; you can redistribute it and/or modify it under
  the terms of the Lesser GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  HSeq is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
  License for more details.

  You should have received a copy of the Lesser GNU General Public
  License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
  ----*)

(*
 * Identifier and theorem records
 *)

type property = string
let simp_property = "simp"
module Tree = Treekit.StringTree

type id_record =
    {
      typ: Gtype.t;
      def: Logic.thm option;
      dprops: property list
    }

type thm_record =
    {
      thm: Logic.thm;
      props: property list
    }

type thy =
    {
      name: string;
      marker: Scope.marker;
      protection: bool;
      date: float;
      parents: string list;
      lfiles: string list;
      axioms: (thm_record) Tree.t;
      theorems: (thm_record) Tree.t;
      defns: (id_record) Tree.t;
      typs: (Scope.type_record) Tree.t;
      type_pps: (string * Printerkit.record) list;
      id_pps: (string * (Printerkit.record * Parser.sym_pos)) list;
      pp_syms: (string * string) list;
    }

type contents=
    {
      cname: string;
      cmarker: Scope.marker;
      cprotection: bool;
      cdate: float;
      cparents: string list;
      cfiles: string list;
      caxioms: (string * thm_record) list;
      ctheorems: (string * thm_record) list;
      cdefns: (string * id_record) list;
      ctyps: (string * Scope.type_record) list;
      ctype_pps: (string * Printerkit.record) list;
      cid_pps: (string * (Printerkit.record * Parser.sym_pos)) list;
      cpp_syms: (string * string) list;
    }

let get_date thy = thy.date
let set_date thy = { thy with date = Lib.date() }

let mk_thy n ps =
  let thy =
    {
      name = n;
      marker = Scope.mk_marker n;
      protection = false;
      date = 0.0;
      parents = ps;
      lfiles = [];
      axioms = Tree.empty;
      theorems = Tree.empty;
      defns = Tree.empty;
      typs = Tree.empty;
      type_pps = [];
      id_pps = [];
      pp_syms = [];
    }
  in
  set_date thy

let flatten_list l =
  let rec flatten_aux ls rs =
      match ls with
      | [] -> rs
      | (n::_)::xs -> flatten_aux xs (n::rs)
      | [] :: xs -> flatten_aux xs rs
  in
  flatten_aux l []

let contents thy =
  {
    cname = thy.name;
    cmarker = thy.marker;
    cprotection = thy.protection;
    cdate = thy.date;
    cparents = thy.parents;
    cfiles = thy.lfiles;
    caxioms = Tree.to_list thy.axioms;
    ctheorems = Tree.to_list thy.theorems;
    cdefns = Tree.to_list thy.defns;
    ctyps = Tree.to_list thy.typs;
    ctype_pps = thy.type_pps;
    cid_pps = thy.id_pps;
    cpp_syms = thy.pp_syms;
  }

(*** Basic Theory Operations ***)

let get_name thy = thy.name
let set_name thy x = { thy with name = x }
let get_marker thy = thy.marker
let set_marker thy x = { thy with marker = x }
let get_parents thy = thy.parents
let set_parents thy x = { thy with parents = x }
let get_protection thy = thy.protection
let get_files thy = thy.lfiles
let set_files thy x = { thy with lfiles = x }

let add_parent n thy =
  if (not thy.protection) && (n<>"") && (not (List.mem n (thy.parents)))
  then set_parents thy (n::(thy.parents))
  else raise (Report.error ("add_parent: "^n))

let rec add_parents ns thy =
  List.fold_left (fun th n -> add_parent n th) thy (List.rev ns)

let set_protection thy =
  let thy1 = { thy with protection = true } in
  set_date thy1

let add_file f thy =
  set_files thy (f::(get_files thy))

let remove_file f thy =
  set_files thy
    (List.filter
       (fun x -> (String.compare x f) <> 0) (get_files thy))

(*
 * Theory Components
 *)

(*** Axioms and theorems ***)

let get_axioms thy = thy.axioms
let set_axioms thy x = { thy with axioms = x }

let get_axiom_rec n thy =
  try Tree.find thy.axioms n
  with Not_found ->
    raise (Report.error
             ("Axiom "^n^" not found in theory "^(get_name thy)^"."))

let get_axiom n thy =
  let ar = get_axiom_rec n thy
  in
  ar.thm

let add_axiom n ax ps thy =
  if not (get_protection thy)
  then
    if not (Tree.mem thy.axioms n)
    then
      let rcrd= { thm = ax; props = ps }
      in
      set_axioms thy (Tree.add (get_axioms thy) n rcrd)
    else raise (Report.error ("Axiom "^n^" exists"))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let set_axiom_props n ps thy =
  if not (get_protection thy)
  then
    let ar = get_axiom_rec n thy in
    let nar= { ar with props=ps }
    in
    set_axioms thy (Tree.replace (thy.axioms) n nar)
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let get_theorems thy = thy.theorems
let set_theorems thy x = { thy with theorems = x }

let get_theorem_rec n thy =
  Tree.find (get_theorems thy) n

let get_theorem n thy =
  let tr =
    try get_theorem_rec n thy
    with Not_found ->
      raise (Report.error
               ("Theorem "^n^" not found in theory "^(get_name thy)^"."))
  in
  tr.thm

let add_thm n t ps thy =
  if not (get_protection thy)
  then
    if not (Tree.mem thy.theorems n)
    then
      let rcrd= { thm = t; props = ps }
      in
      set_theorems thy (Tree.add (get_theorems thy) n rcrd)
    else raise (Report.error ("Theorem "^n^" exists"))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let set_theorem_props n ps thy =
  if not (get_protection thy)
  then
    match Lib.try_find (get_theorem_rec n) thy with
      | Some(tr) ->
          let ntr = { tr with props = ps }
          in
          set_theorems thy (Tree.replace (thy.theorems) n ntr)
      | _ ->
        raise (Report.error
                 ("Theorem "^n^" not found in theory "^(get_name thy)^"."))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))


(*** Type declarations and definitions ***)

let get_typs thy = thy.typs
let set_typs thy x = { thy with typs = x }
let get_type_rec n thy = Tree.find (get_typs thy) n

let add_type_rec tr thy =
  let dest_tydef tydef =
    if Logic.Defns.is_typealias tydef
    then Logic.Defns.dest_typealias tydef
    else
      if Logic.Defns.is_subtype tydef
      then
        let ctyrec = Logic.Defns.dest_subtype tydef
        in
        (ctyrec.Logic.Defns.type_name,
         ctyrec.Logic.Defns.type_args,
         None)
      else
        raise
          (Report.error "Theory.add_type_rec: Expected a type definition")
  in
  if not (get_protection thy)
  then
    let (lid, args, df) = dest_tydef tr in
    let id = Ident.name_of lid in
    let tr = Scope.mk_type_record id args df
    in
    if not (Tree.mem (get_typs thy) id)
    then set_typs thy (Tree.add (get_typs thy) id tr)
    else raise (Report.error ("Type "^id^" exists"))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let type_exists n thy =
  try ignore(get_type_rec n thy); true
  with Not_found -> false

(*** Term declarations and definitions ***)

let get_defns thy = thy.defns
let set_defns thy xs = { thy with defns = xs }

let get_defn_rec n thy =
  let rcrd = Tree.find (get_defns thy) n
  in
  {
    typ = Gtype.rename_type_vars (rcrd.typ);
    def = rcrd.def;
    dprops = rcrd.dprops
  }

let get_defn n thy =
  let r = get_defn_rec n thy
  in
  match r.def with
      None -> raise (Report.error ("No definition for "^n))
    | Some(d) -> d

let get_id_type n thy =
  let r = get_defn_rec n thy
  in
  r.typ

let id_exists n thy =
  try ignore(get_defn_rec n thy); true
  with Not_found -> false

let set_defn_props n ps thy =
  if not (get_protection thy)
  then
    let dr = get_defn_rec n thy in
    let ndr = { dr with dprops=ps }
    in
    set_defns thy (Tree.replace (get_defns thy) n ndr)
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let add_defn_rec n ty d prop thy =
  if not (get_protection thy)
  then
    if id_exists n thy
    then raise (Report.error ("Identifier "^n^" already exists in theory"))
    else
      let new_record = { typ=ty; def=d; dprops=prop }
      in
      set_defns thy (Tree.add (get_defns thy) n new_record)
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let add_decln_rec n ty props thy =
  add_defn_rec n ty None props thy

(*** Printer-Parser records ***)

let get_symbols thy = thy.pp_syms
let set_symbols thy ls = { thy with pp_syms = ls }
let add_symbol thy p = { thy with pp_syms = (p::(get_symbols thy)) }

let get_id_pps thy = thy.id_pps
let set_id_pps thy x = { thy with id_pps = x }

let get_id_pps thy = thy.id_pps
let set_id_pps thy x = { thy with id_pps = x }

let get_type_pps thy = thy.type_pps
let set_type_pps thy x =  { thy with type_pps = x }

let get_term_pp_rec n thy = List.assoc n thy.id_pps

let add_term_pp_rec n ppr thy=
  if not (get_protection thy)
  then
    if Tree.mem (get_defns thy) n
    then set_id_pps thy (Lib.insert (<) n ppr thy.id_pps)
    else raise (Report.error
                  ("No name "^n^" defined in theory "^(get_name thy)))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let remove_term_pp_rec n thy =
  set_id_pps thy (Lib.filter (fun (x, _) -> x=n ) (get_id_pps thy))

let get_term_pplist thy =
  let ppl = get_id_pps thy
  and tn = thy.name
  in
  List.map (fun (x, y) -> (Ident.mk_long tn x, y)) ppl

let add_term_ppinfo thy pp =
  let ppl = get_id_pps thy
  and tn = thy.name
  in
  List.fold_left
    (fun ppi (i, r) ->
      (Printers.add_term_record ppi (Ident.mk_long tn i) (fst r)))
        pp ppl

let get_type_pp_rec n thy = List.assoc n thy.type_pps

let add_type_pp_rec n ppr thy=
  if not (get_protection thy)
  then
    if Tree.mem thy.typs n
    then set_type_pps thy (Lib.insert (<) n ppr (get_type_pps thy))
    else raise (Report.error
                  ("No type "^n^" defined in theory "^(get_name thy)))
  else raise (Report.error ("Theory "^(get_name thy)^" is protected"))

let remove_type_pp_rec n thy =
  set_type_pps thy (Lib.filter (fun (x, _) -> x = n) (get_type_pps thy))

let get_type_pplist thy =
  let ppl = get_type_pps thy
  and tn = thy.name
  in
  List.map (fun (x, y) -> (Ident.mk_long tn x, y)) ppl

let add_type_ppinfo thy pp =
  let ppl = get_type_pps thy
  and tn = thy.name
  in
  List.fold_left
    (fun ppi (i, r) ->
      (Printers.add_type_record ppi (Ident.mk_long tn i) r))
        pp ppl

(*
 * Theory Storage
 *)

(** Representation for saving to disk. *)

type id_save_record =
    {
      sty: Gtype.stype;
      sdef: Logic.saved_thm option;
      sdprops: property list
    }

let to_save ir =
  let sdef_record =
    match ir.def with
      | None -> None
      | Some(d) -> Some (Logic.to_save d)
  in
  {
    sty=Gtype.to_save ir.typ;
    sdef = sdef_record;
    sdprops = ir.dprops
  }

let from_save scp sr =
  let def_record =
    match sr.sdef with
      | None -> None
      | Some(d) -> Some(Logic.from_save scp d)
  in
  {
    typ=Gtype.from_save sr.sty;
    def = def_record;
    dprops = sr.sdprops
  }

type thm_save_record =
    {
      sthm: Logic.saved_thm;
      sprops: property list
    }

let thm_to_save tr=
  {
    sthm = Logic.to_save tr.thm;
    sprops = tr.props
  }

let thm_from_save scp sr=
  {
    thm = Logic.from_save scp sr.sthm;
    props = sr.sprops
  }

(** Representation of a theory stored on disk. *)
type saved_thy =
    {
      sname: string;
      sprot: bool;
      sdate: float;
      sparents: string list;
      sfiles: string list;
      saxioms: (string * thm_save_record) list;
      stheorems: (string * thm_save_record) list;
      sdefns: (string * id_save_record) list;
      stypes: (string * Gtype.stypedef_record) list;
      stype_pps: (string * Printerkit.record) list;
      sid_pps: (string * (Printerkit.record * Parser.sym_pos)) list;
      spp_syms: (string * string) list;
    }

let saved_name sthy = sthy.sname
let saved_parents sthy = sthy.sparents
let saved_prot sthy = sthy.sprot
let saved_date sthy = sthy.sdate

(** [new_thy_scope thy scp]: Extend [scp] with the name and marker of
    [thy].  Does not add other contents of [thy] to the scope.
*)
let new_thy_scope thy scp =
  let mark = get_marker thy
  and name = get_name thy
  in
  let test_thy_scope n =
    (String.compare name n) = 0 || Scope.in_scope scp n
  and test_marker_scope m =
    Tag.equal mark m || Scope.in_scope_marker scp m
  in
  {
    scp with
    Scope.curr_thy = mark;
    thy_in_scope = test_thy_scope;
    marker_in_scope = test_marker_scope;
  }

(** Make a theory from a saved theory. *)
let from_saved scp sthy =
  let unsave f xs =
    List.fold_left
      (fun tr (k, d) -> Tree.add tr k d)
      Tree.empty
      (List.map (fun (x, y) -> (x, f y)) xs)
  in
  let name = sthy.sname in
  let thy = mk_thy name (sthy.sparents)
  and tydefs_list =
    List.map (fun (x, y) -> (x, Gtype.from_save_rec y)) sthy.stypes
  in
  let thy_scp =
    let scp1 = new_thy_scope thy scp
    and new_tydefs_list =
      List.map (fun (id, rd) -> (Ident.mk_long name id, rd)) tydefs_list
    in
    let scp2 = Scope.extend_with_typedefs scp1 new_tydefs_list in
    let new_defns =
      List.map
        (fun (id, rd) -> Ident.mk_long name id, Gtype.from_save rd.sty)
        sthy.sdefns
    in
    Scope.extend_with_terms scp2 new_defns
  in
  let prot = sthy.sprot
  and tim = sthy.sdate
  and prnts = sthy.sparents
  and lfls = sthy.sfiles
  and axs = unsave (thm_from_save thy_scp) sthy.saxioms
  and thms = unsave (thm_from_save thy_scp) sthy.stheorems
  and defs = unsave (from_save thy_scp) sthy.sdefns
  and tydefs_table =
    List.fold_left
      (fun tr (k, d) -> Tree.add tr k d)
      Tree.empty tydefs_list
  and ntype_pps = sthy.stype_pps
  and nid_pps = sthy.sid_pps
  and npp_syms = sthy.spp_syms
  in
  {
    thy with
      protection = prot;
      date = tim;
      parents = prnts;
      lfiles = lfls;
      axioms = axs;
      theorems = thms;
      defns = defs;
      typs = tydefs_table;
      type_pps = ntype_pps;
      id_pps = nid_pps;
      pp_syms = npp_syms;
  }

(*** Primitive input/output of theories ***)

let output_theory oc thy =
  let tree_to_list xs = Tree.to_list xs in
  let mk_save f xs = List.map (fun (x, y) -> (x, f y)) xs
  in
  let saxs = mk_save thm_to_save (tree_to_list thy.axioms)
  and sthms = mk_save thm_to_save (tree_to_list thy.theorems)
  and sdefs = mk_save to_save (tree_to_list thy.defns)
  and stypes = mk_save Gtype.to_save_rec (tree_to_list thy.typs)
  and styp_pps = thy.type_pps
  and sid_pps = thy.id_pps
  and spp_syms = thy.pp_syms
  in
  output_value oc
    (thy.name, thy.protection, thy.date, thy.parents, thy.lfiles,
     saxs, sthms, sdefs, stypes, styp_pps, sid_pps, spp_syms)

let input_theory ic =
  let (n, prot, tim, prnts, lfls, saxs, sthms,
       sdefs, stys, ntype_pps, nid_pps, npp_syms) = input_value ic
  in
  {
    sname = n;
    sprot = prot;
    sdate = tim;
    sparents = prnts;
    sfiles = lfls;
    saxioms = saxs;
    stheorems = sthms;
    sdefns = sdefs;
    stypes = stys;
    stype_pps = ntype_pps;
    sid_pps = nid_pps;
    spp_syms = npp_syms;
  }

(*** Toplevel input/output of theories ***)
let load_theory fname =
  let ic = open_in_bin fname in
  let sthy = input_theory ic;
  in
  close_in ic; sthy

let save_theory thy fname=
  let oc = open_out_bin fname
  in
  output_theory oc thy;
  close_out oc

let end_theory thy prot =
  if (get_protection thy) || (not prot)
  then thy
  else set_protection thy

(*
 * Pretty-Printer
 *)

let print_property pp p =
  Format.printf "@[%s@]" p

let print_properties pp ps =
  match ps with
    | [] -> ()
    | _ ->
      Format.printf "@[(";
      Printerkit.print_list
        ((fun p -> print_property pp p),
         (fun _ -> Format.printf ",@ ")) ps;
      Format.printf ")@]@,"

(** Theory printer **)

let print_section title =
  Format.printf "@[-----\n%s\n-----@]@." title

let print_protection p =
  if p
  then ()
  else Format.printf "@[read-write@]@,"
and print_date d =
  Format.printf "@[Date: %f@]@." d
and print_parents ps =
  Format.printf "@[<2>Parents: ";
  begin
    match ps with
      | [] -> (Format.printf "%s" "None")
      | _ ->
        Printerkit.print_list
          ((fun s -> Format.printf "%s" s),
           (fun _ -> Format.printf "@ ")) ps
  end;
  Format.printf "@]@."
and print_files ps =
  begin
    match ps with
      | [] -> ()
      | _ ->
         Format.printf "@[<2>Libraries: ";
         Printerkit.print_list
           ((fun s -> Format.printf "%s" s),
            (fun _ -> Format.printf "@ ")) ps
  end;
  Format.printf "@]"
and print_thms pp n ths =
  let sorted_ths =
    let comp (x, _) (y, _) = compare x y
    in
    List.sort comp ths
  in
  print_section n;
  Format.printf "@[<v>";
  Printerkit.print_list
    ((fun (tn, t) ->
      begin
        Format.printf "@[<2>%s:@ " tn;
        print_properties pp t.props;
        Format.printf "@,";
        Logic.print_thm pp t.thm;
        Format.printf "@]@,"
      end),
     (fun _ -> ()))
    sorted_ths;
  Format.printf "@]"
and print_tydefs pp n tys =
  let sorted_tys =
    let comp (x, _) (y, _) = compare x y
    in
    List.sort comp tys
  in
  print_section n;
  Format.printf "@[<v>";
  Printerkit.print_list
    ((fun (n, tyd) ->
      Format.printf "@[<2>";
      let (_, args, alias) = Scope.dest_type_record tyd in
      begin
        if args = []
        then ()
        else
          begin
            Format.printf "(";
            Printerkit.print_list
              ((fun s -> Format.printf "'%s" s),
               (fun _ -> Format.printf ",@ "))
              args;
            Format.printf ")"
          end
      end;
      Format.printf "%s@," n;
      begin
        if alias = None
        then ()
        else
          begin
            Format.printf "=@,";
            Printers.print_type pp (Lib.from_some alias)
          end
      end;
      Format.printf "@]@."),
     (fun _ -> ())) sorted_tys;
  Format.printf "@]"
and print_defs pp n defs =
  let sorted_defs =
    let comp (x, _) (y, _) = compare x y
    in
    List.sort comp defs
  in
  print_section n;
  Format.printf "@[<v>";
  Printerkit.print_list
    ((fun (n, d) ->
      Format.printf "@[<2>%s:@ " n;
      print_properties pp d.dprops;
      Format.printf "@ ";
      Printers.print_type pp d.typ;
      Format.printf "@ ";
      begin
        match d.def with
          | None -> ()
          | Some(df) ->
            Logic.print_thm pp df
      end;
      Format.printf "@]@."),
     (fun _ -> ())) sorted_defs;
  Format.printf "@]"

let print_term_pps n pps =
  let print_pos pos =
    match pos with
      | Lib.First -> ()
      | Lib.Last ->
        Format.printf "@[position=@ Last@]"
      | Lib.Before id ->
        Format.printf "@[position=@ Before@ @[";
        Printerkit.print_ident id;
        Format.printf "@]@]@ "
      | Lib.After id ->
        Format.printf "@[position=@ After@ @[";
        Printerkit.print_ident id;
        Format.printf "@]@]@ "
      | Lib.Level id ->
        Format.printf "@[position=@ Level@ @[";
        Printerkit.print_ident id;
        Format.printf "@]@]@ "
  in
  let sorted_pps =
    let comp (x, _) (y, _) = compare x y
    in
    List.sort comp pps
  in
  print_section n;
  Format.printf "@[<v>";
  Printerkit.print_list
    ((fun (n, (r, p)) ->
      Format.printf "@[<2>%s@ " n;
      begin
        match (r.Printerkit.repr) with
            None -> ()
          | Some(s) -> Format.printf "\"%s\"@ " s
      end;
      Format.printf "precedence = %i@ " r.Printerkit.prec;
      Format.printf "fixity = %s@ "
        (Printerkit.fixity_to_string r.Printerkit.fixity);
      print_pos p;
      Format.printf "@]@,"),
     (fun _ -> ())) sorted_pps;
  Format.printf "@]"

let print_type_pps n pps =
  let sorted_pps =
    let comp (x, _) (y, _) = compare x y
    in
    List.sort comp pps
  in
  print_section n;
  Format.printf "@[<v>";
  Printerkit.print_list
    ((fun (n, r) ->
      Format.printf "@[<2>%s@ " n;
      begin
        match (r.Printerkit.repr) with
          | None -> ()
          | Some(s) -> Format.printf "\"%s\"@ " s
      end;
      Format.printf "precedence = %i@ " r.Printerkit.prec;
      Format.printf "fixity = %s@ "
        (Printerkit.fixity_to_string r.Printerkit.fixity);
      Format.printf "@]@,"),
     (fun _ -> ())) sorted_pps;
  Format.printf "@]"

let print_pp_syms n pps =
  let print_sym (s, t) = Format.printf "\"%s\":\"%s\"" s t in
  begin
    print_section n;
    Format.printf "@[<v>";
    Printerkit.print_list (print_sym, (fun _ -> Format.printf "@ ")) pps;
    Format.printf "@]@."
  end

let print ppstate thy =
  let print_list p l = if l <> [] then p l else ()
  in
  let content = contents thy
  in
  Format.printf "@[<v>@.";
  Format.printf "@[-------------@]@.";
  Format.printf "@[%s@]@." content.cname;
  print_parents content.cparents;
  print_files content.cfiles;
  print_date content.cdate;
  print_protection content.cprotection;
  print_list (print_tydefs ppstate "Types") content.ctyps;
  print_list (print_thms ppstate "Axioms") content.caxioms;
  print_list (print_defs ppstate "Definitions") content.cdefns;
  print_list (print_thms ppstate "Theorems") content.ctheorems;
  print_list (print_type_pps "Type printer/parser information")
             content.ctype_pps;
  print_list (print_term_pps "Term printer/parser information")
             content.cid_pps;
  print_list (print_pp_syms "Parser symbols") content.cpp_syms;
  Format.printf "@[-------------@]";
  Format.printf "@]"
