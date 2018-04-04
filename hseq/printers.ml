(*----
  Name: printers.ml
  Copyright Matthew Wahab 2018
  Author: Matthew Wahab <mwb.cde@gmail.com>

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

(*
 * Combined printer information tables}
 *)

open Printerkit
open Basic

(** The combined printer information for terms and types. *)
type ppinfo =
    {
      terms: (ppinfo, (Basic.term * (Basic.term)list))info;
      types: (ppinfo, (Ident.t * (Gtype.t)list))info
    }

let mk_ppinfo sz =
  {
    terms = mk_info sz;
    types = mk_info sz;
  }
let empty_ppinfo() = mk_ppinfo default_info_size

(** Operations involving term identifiers *)

type term_printer =
  ppinfo -> (fixity * int) -> (Basic.term * Basic.term list) printer

let get_term_info info x = get_info (info.terms) x
let set_term_info info x = {info with terms = x}
let add_term_info info id prec fixity repr =
  set_term_info info (add_info (info.terms) id prec fixity repr)
let add_term_record info id record =
  set_term_info info (add_record (info.terms) id record)
let remove_term_info info id =
  set_term_info info (remove_info (info.terms) id)

let get_term_printer info x = get_printer (info.terms) x
let add_term_printer info id prnt =
  set_term_info info (add_printer (info.terms) id prnt)
let remove_term_printer info id =
  set_term_info info (remove_printer (info.terms) id)

(** Operations involving type identifiers *)
type gtype_printer =
  ppinfo -> (fixity * int) -> (Ident.t * (Gtype.t list)) printer
let get_type_info info x = get_info (info.types) x
let set_type_info info x = {info with types = x}
let add_type_info info id prec fixity repr =
  set_type_info info (add_info (info.types) id prec fixity repr)
let add_type_record info id record =
  set_type_info info (add_record (info.types) id record)
let remove_type_info info id =
  set_type_info info (remove_info (info.types) id)

let get_type_printer info x = get_printer (info.types) x
let add_type_printer info id prnt =
  set_type_info info (add_printer (info.types) id prnt)
let remove_type_printer info id =
  set_type_info info (remove_printer (info.types) id)

(** Precedence of function application *)
let fun_app_prec = 90

(** Precedence of Quantifiers *)
let prec_qnt q =
  match q with
      Lambda -> 60
    | All -> 55
    | Ex -> 55
    | _ -> 55

(** Associativity of Quantifiers *)
let assoc_qnt q =
  match q with
      Lambda -> non_assoc
    | All -> non_assoc
    | Ex -> non_assoc
    | _ -> non_assoc

(** Fixity of Quantifiers *)
let fixity_qnt q = nonfix

(** {5 Type printers} *)

module Types =
  struct

    let pplookup ppstate id =
      try Printerkit.get_record (ppstate.types) id
      with Not_found ->
        Printerkit.mk_record
          Printerkit.default_type_prec
          Printerkit.default_type_fixity
          None

    let print_bracket = Printerkit.print_assoc_bracket

    let rec print_type ppstate pr t =
      let rec print_aux ppstate pr x =
        match x with
        | Gtype.Atom(Gtype.Var(_)) ->
           Format.printf "@[<hov 2>'%s@]" (Gtype.get_var_name x)
        | Gtype.Atom(Gtype.Weak(_)) ->
           Format.printf "@[<hov 2>_%s@]" (Gtype.get_weak_name x)
        | Gtype.Atom(Gtype.Ident(op)) -> print_app ppstate pr (op, [])
        | Gtype.App(_) ->
           let op, args = Gtype.dest_constr x in
           print_app ppstate pr (op, args)
      and print_infix (assoc, prec) (nassoc, nprec) (f, args) =
        begin
          match args with
          | [] ->
             (* Print '(OP)' *)
             Format.printf "@[";
             Printerkit.print_identifier (pplookup ppstate) f;
             Format.printf "@]"
          | (lf::lr::rs) ->
             (* Print '(<arg> OP <arg>) <rest>' *)
             Format.printf "@[<hov 2>";
             print_bracket (assoc, prec) (nassoc, nprec) "(";
             print_aux ppstate (Printerkit.infixl, nprec) lf;
             Printerkit.print_space();
             Printerkit.print_identifier (pplookup ppstate) f;
             Printerkit.print_space();
             print_aux ppstate (Printerkit.infixr, nprec) lr;
             Printerkit.print_list
               (print_type ppstate (nassoc, nprec),
                Printerkit.print_space)
               rs;
             print_bracket (assoc, prec) (nassoc, nprec) ")";
             Format.printf "@]"
          | (lf::rs) ->
             (* Print '(<arg> OP) <rest>' *)
             Format.printf "@[<hov 2>";
             print_bracket (assoc, prec) (nassoc, nprec) "(";
             print_type ppstate (Printerkit.infixl, nprec) lf;
             Printerkit.print_space();
             Printerkit.print_identifier (pplookup ppstate) f;
             Printerkit.print_list
               (print_type ppstate (nassoc, nprec),
                Printerkit.print_space)
               rs;
             print_bracket (assoc, prec) (nassoc, nprec) ")";
             Format.printf "@]"
        end
      and print_suffix (assoc, prec) (nassoc, nprec) (f, args) =
        begin
          Format.printf "@[<hov 2>";
          print_bracket (assoc, prec) (nassoc, nprec) "(";
          Printerkit.print_suffix
            ((fun pr -> Printerkit.print_identifier (pplookup ppstate)),
             (fun pr l ->
               if l = [] then ()
               else
                 Printerkit.print_sep_list
                   (print_type ppstate (assoc, pr), ",") l))
            nprec (f, args);
          print_bracket (assoc, prec) (nassoc, nprec) ")";
          Format.printf "@]"
        end
      and print_prefix (assoc, prec) (nassoc, nprec) (f, args) =
        Format.printf "@[<hov 2>";
        if args = [] then ()
        else
          begin
            Printerkit.print_string "(";
            Printerkit.print_sep_list (print_type ppstate (assoc, prec), ",") args;
            Format.printf ")@,"
          end;
        Printerkit.print_identifier (pplookup ppstate) f;
        Format.printf "@]"
      and print_app ppstate (assoc, prec) (f, args) =
        let pprec = pplookup ppstate f in
        let nfixity = pprec.Printerkit.fixity in
        let (nassoc, nprec) = (nfixity, pprec.Printerkit.prec)
        in
        let some_printer =
          Lib.try_find (Printerkit.get_printer (ppstate.types)) f
        in
        if some_printer <> None
        then (Lib.from_some some_printer) ppstate (assoc, prec) (f, args)
        else
          if Printerkit.is_infix nfixity
          then print_infix (assoc, prec) (nassoc, nprec) (f, args)
          else
            if Printerkit.is_suffix nfixity
            then print_suffix (assoc, prec) (nassoc, nprec) (f, args)
            else print_prefix (assoc, prec) (nassoc, nprec) (f, args)
      in
      print_aux ppstate pr t

  end

let print_type ppinfo ty =
  Types.print_type
    ppinfo
    (Printerkit.default_type_fixity, Printerkit.default_type_prec) ty

let print_type_error fmt pinfo err =
  Format.fprintf fmt "@[%s@ " err.Gtype.msg;
  Printerkit.print_sep_list (print_type pinfo, ",") err.Gtype.typs;
  Format.fprintf fmt "@]@."

(** {5 Term printers} *)

module Terms =
  struct
    let print_bracket  = Printerkit.print_assoc_bracket

    let pplookup ppstate id =
      try Printerkit.get_record ppstate.terms id
      with Not_found ->
        Printerkit.mk_record
          Printerkit.default_term_prec
          Printerkit.default_term_fixity
          None

    let print_meta qnt =
      let _, qv, qty = dest_binding qnt
      in
      Format.printf "@[(_%s:@ %s)@]" qv (Gtype.string_gtype qty)

    let print_typed_obj level printer ppstate prec (obj, ty) =
      if (!Settings.print_type_level > level)
      then
        (Format.printf "@[<2>(";
         printer ppstate prec obj;
         Format.printf ":@ ";
         print_type (ppstate) ty;
         Format.printf ")@]")
      else
        (Format.printf "@[<2>";
         printer ppstate prec obj;
         Format.printf "@]")

    let print_typed_identifier ppstate (id, ty) =
      let printer ppstate _ i =
        Printerkit.print_identifier (pplookup ppstate) i
      in
      print_typed_obj 3 printer ppstate (Printerkit.nonfix, 0) (id, ty)

    let print_typed_name ppstate (id, ty) =
      let printer ppstate _ n = Format.print_string n
      in
      print_typed_obj 2 printer ppstate (Printerkit.nonfix, 0) (id, ty)

    let print_ident_as_identifier ppstate (f, p) v =
      let (id, ty) = Term.dest_ident v
      in
      Printerkit.print_identifier (pplookup ppstate) id

    let print_prefix (opr, tpr) (assoc, prec) (f, args) =
      Format.printf "@[<2>";
      opr (assoc, prec) f;
      Format.printf "@ ";
      Printerkit.print_list (tpr (assoc, prec), Printerkit.print_space) args;
      Format.printf "@]"

    let print_suffix (opr, tpr) (assoc, prec) (f, args) =
      Format.printf "@[<2>";
      Printerkit.print_list (tpr (assoc, prec), Printerkit.print_space) args;
      Format.printf "@ ";
      opr (assoc, prec) f;
      Format.printf "@]"

    let rec print_infix (opr, tpr) (assoc, prec) (f, args) =
      Format.printf "@[<2>";
      begin
        match args with
        | [] ->
           Format.printf "@[<2>";
           opr (assoc, prec) f;
           Format.printf "@]"
        | l::r::rest ->
           Format.printf "@[<2>";
           tpr (Printerkit.infixl, prec) l;
           Format.printf "@ ";
           opr (assoc, prec) f;
           Format.printf "@ ";
           tpr (Printerkit.infixr, prec) r;
           Printerkit.print_list (tpr (assoc, prec), Printerkit.print_space) rest;
           Format.printf "@]"
        | l::rest ->
           Format.printf "@[<2>";
           tpr (Printerkit.infixl, prec) l;
           Format.printf "@ ";
           opr (assoc, prec) f;
           Printerkit.print_list (tpr (assoc, prec), Printerkit.print_space) rest;
           Format.printf "@]"
      end;
      Format.printf "@]"

    let print_fn_app ppstate (fnpr, argpr) (assoc, prec) (f, args) =
      let (id, ty) = Term.dest_ident f in
      let pprec =
        try Printerkit.get_record ppstate.terms id
        with Not_found ->
          Printerkit.mk_record
            fun_app_prec
            Printerkit.default_term_fixity
            None
      in
      let (nfixity, nprec) = (pprec.Printerkit.fixity, pprec.Printerkit.prec) in
      let user_printer=
        try Some (Printerkit.get_printer (ppstate.terms) id)
        with Not_found -> None
      and std_printer =
        if (Printerkit.is_infix nfixity)
        then print_infix (fnpr, argpr)
        else
          if (Printerkit.is_suffix nfixity)
          then print_suffix (fnpr, argpr)
          else print_prefix (fnpr, argpr)
      in
      Format.printf "@[<2>";
      begin
        match user_printer with
        | None ->
           print_bracket (assoc, prec) (nfixity, nprec) "(";
           std_printer (nfixity, nprec) (f, args);
           print_bracket (assoc, prec) (nfixity, nprec) ")"
        | Some p -> p ppstate (assoc, prec) (f, args)
      end;
      Format.printf "@]"

    let print_typed_term tpr ppstate (assoc, prec) (trm, ty)=
      Format.printf "@[<2>(";
      tpr ppstate (assoc, prec) trm;
      Format.printf ":@ ";
      print_type ppstate ty;
      Format.printf ")@]"

    let print_qnt ppstate q =
      let _, qvar, qtyp = dest_binding q
      in
      print_typed_name ppstate (qvar, qtyp)

    let print_qnts ppstate prec (qnt, qs) =
      Format.printf "@[%s" qnt;
      Printerkit.print_list (print_qnt ppstate, Printerkit.print_space) qs;
      Format.printf":@]"

    let rec print_term ppstate (assoc, prec) x =
      match x with
        Id(n, ty) ->
        let user_printer=
          try Some (Printerkit.get_printer (ppstate.terms) n)
          with Not_found -> None
        in
        (match user_printer with
           None -> print_typed_identifier ppstate (n, ty)
         | Some(p) -> p ppstate (assoc, prec) (x, []))
      | Free(n, ty) ->
         print_typed_name ppstate (n, ty)
      | Bound(n) ->
         Format.printf "@[%s@]" ((Term.get_binder_name x))
      | Meta(n) ->
         Format.printf "@[%s@]" ((Term.get_binder_name x))
      | Const(c) ->
         Format.printf "@[%s@]" (Basic.string_const c);
      | App(t1, t2) ->
         let f, args = Term.get_fun_args x
         in
         if Term.is_ident f
         then
           begin
             Format.printf "@[";
             print_fn_app ppstate
                          (print_ident_as_identifier ppstate,
                           (fun (a, p) t-> print_term ppstate (a, p) t))
                          (assoc, prec) (f, args);
             Format.printf "@]"
           end
         else
           begin
             let (tassoc, tprec) =
               (Printerkit.default_term_fixity, fun_app_prec)
             in
             Format.printf "@[<hov 2>";
             print_bracket (assoc, prec) (tassoc, tprec) "(";
             print_prefix
               (print_term ppstate, print_term ppstate)
               (tassoc, tprec) (f, args);
             print_bracket (assoc, prec) (tassoc, tprec) ")";
             Format.printf "@,@]"
           end
      | Qnt(q, body) ->
         let (qnt, qvar, qtyp) = Basic.dest_binding q in
         let (qnts, b) = Term.strip_qnt qnt x in
         let (tassoc, tprec) =
           (fixity_qnt qnt, prec_qnt qnt)
         in
         Format.printf "@[";
         print_bracket (assoc, prec) (tassoc, tprec) "(";
         Format.printf "@[<hov 3>";
         print_qnts ppstate tprec (Basic.quant_string qnt, qnts);
         Printerkit.print_space ();
         print_term ppstate (tassoc, tprec) b;
         Format.printf "@]";
         print_bracket (assoc, prec) (tassoc, tprec) ")";
         Format.printf "@]"

    let print ppstate x =
      Format.open_box 0;
      print_term ppstate
                 (Printerkit.default_term_fixity, Printerkit.default_term_prec)
                 (Term.retype_pretty (Gtype.empty_subst()) x);
      Format.close_box()

    let simple_print_fn_app ppstate (assoc, prec) (f, args) =
      let (id, _) = Term.dest_ident f in
      let pprec = pplookup ppstate id in
      let (nfixity, nprec) = (pprec.Printerkit.fixity, pprec.Printerkit.prec)
      in
      let iprint (a, pr) = print_ident_as_identifier ppstate (a, pr)
      and tprint (a, pr) = print_term ppstate (a, pr)
      in
      Format.printf "@[<2>";
      print_bracket (assoc, prec) (nfixity, nprec) "(";
      if(Printerkit.is_infix nfixity)
      then print_infix (iprint, tprint) (assoc, prec) (f, args)
      else
        if Printerkit.is_suffix nfixity
        then print_suffix (iprint, tprint) (assoc, prec) (f, args)
        else print_prefix (iprint, tprint) (assoc, prec) (f, args);
      print_bracket (assoc, prec) (nfixity, nprec) ")";
      Format.printf "@]"

    (** [print_as_binder (sym_assoc, sym_prec) f sym] Construct a printer
    to print function applications of the form [f (%x: P)] as [sym x:
    P].
     *)
    let print_qnt_body ppstate (assoc, prec) (qs, body) =
      Format.printf "@[";
      Printerkit.print_list (print_qnt ppstate, Printerkit.print_space) qs;
      Format.printf ":@ ";
      print_term ppstate (assoc, prec) body;
      Format.printf"@]"

    let print_as_binder (sym_assoc, sym_prec) ident sym =
      let print_qnt ppstate (assoc, prec) arg =
        let (qnts, body) =
          Term.strip_fun_qnt ident (Term.mk_app (Term.mk_ident ident) arg) []
        in
        Printerkit.print_assoc_bracket (assoc, prec) (sym_assoc, sym_prec) "(";
        Format.printf "@[<hov 3>";
        print_qnts ppstate (sym_assoc, sym_prec) (sym, qnts);
        Printerkit.print_space ();
        print_term ppstate (assoc, sym_prec) body;
        Format.printf "@]";
        Printerkit.print_assoc_bracket (assoc, prec) (sym_assoc, sym_prec) ")"
      in
      let lambda_arg x =
        match x with
        | Basic.Qnt(q, body) -> (Basic.binder_kind q)=Basic.Lambda
        | _ -> false
      in
      let printer ppstate prec (f, args) =
        match args with
        | (a::rest) ->
           Format.printf "@[<2>";
           if(lambda_arg a)
           then print_qnt ppstate prec a
           else simple_print_fn_app ppstate prec (f, args);
           Printerkit.print_list
             (print_term ppstate prec, Printerkit.print_space) rest;
           Format.printf "@]"
        | _ ->
           Format.printf "@[";
           print_ident_as_identifier ppstate prec f;
           Format.printf "@]"
      in
      printer
  end

(** Term printer *)
let print_term = Terms.print

(** Print a term error *)
let print_term_error fmt pinfo err =
  Format.fprintf fmt "@[%s@ " err.Term.msg;
  Printerkit.print_sep_list (print_term pinfo, ",") err.Term.terms;
  Format.fprintf fmt "@]@."




