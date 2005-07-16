(*-----
 Name: theory.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(*
type property = exn
exception SimpProperty
let simp_property = SimpProperty
*)
type property = string
let simp_property = "simp"

type id_record= 
    {
     typ: Basic.gtype; 
     def: Logic.thm option; 
     dprops: property list
   }

type id_save_record= 
    {
     sty: Basic.gtype; 
     sdef: Logic.saved_thm option; 
     sdprops : property list
   }

type thm_record =
    {
     thm: Logic.thm;
     props: property list
   }
type thm_save_record =
    {
     sthm: Logic.saved_thm;
     sprops: property list
   }

type thy = 
    {
     name: string;
     mutable protection: bool;
     mutable date: float;
     mutable parents:  string list;
     mutable lfiles: string list;
     axioms: (string, thm_record) Hashtbl.t;
     theorems: (string, thm_record) Hashtbl.t;
     defns: (string, id_record) Hashtbl.t;
     typs: (string, Gtypes.typedef_record) Hashtbl.t;
     mutable type_pps: (string * Printer.record) list;
     mutable id_pps: (string * Printer.record) list
   }

type contents=
    {
     cname: string;
     cprotection: bool;
     cdate: float;
     cparents: string list;
     cfiles: string list;
     caxioms: (string * thm_record) list;
     ctheorems: (string * thm_record) list;
     cdefns: (string * id_record) list;
     ctyps: (string * Gtypes.typedef_record) list;
     ctype_pps: (string * Printer.record) list;
     cid_pps: (string * Printer.record) list 
   }

let get_date thy = thy.date
let set_date thy = thy.date<-Lib.date()

let mk_thy n = 
  let thy = {name=n; 
	     protection=false;
	     date=0.0;
	     parents=[];
	     lfiles = [];
	     axioms=Hashtbl.create 1;
	     theorems=Hashtbl.create 1;
	     defns=Hashtbl.create 1; 
	     typs = Hashtbl.create 1;
	     type_pps = [];
	     id_pps = []  }
  in 
  set_date thy; thy


let get_name thy = thy.name
let get_parents thy = thy.parents
let add_parent n thy =
  if (not thy.protection) & (n<>"") & (not (List.mem n (thy.parents)))
  then thy.parents<-(n::(thy.parents))
  else raise (Result.error ("add_parent: "^n))

let add_parents ns thy =
  match ns with
    [] -> ()
  |	(x::xs) -> add_parent x thy

let get_protection thy = thy.protection
let set_protection thy = 
  thy.protection<-true; set_date thy

let get_files thy = thy.lfiles
let set_files fs thy = thy.lfiles <- fs
let add_file f thy = set_files (f::(get_files thy)) thy
let remove_file f thy = 
  set_files 
    (List.filter (fun x -> (String.compare x f) != 0) (get_files thy)) thy

let add_pp_rec idsel n ppr thy=
  if not (get_protection thy)
  then 
    if idsel = Basic.fn_id 
    then 
      (if Lib.member n thy.defns
      then thy.id_pps <- (Lib.insert n ppr thy.id_pps)
      else raise (Result.error 
		    ("No name "^n^" defined in theory "^(get_name thy))))
    else 
      (if Lib.member n thy.typs
      then thy.type_pps <- (Lib.insert n ppr thy.type_pps)
      else raise (Result.error
		    ("No type "^n^" defined in theory "^(get_name thy))))
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))

let get_pp_rec idsel n thy =
  if idsel = Basic.fn_id 
  then List.assoc n thy.id_pps
  else List.assoc n thy.type_pps

let remove_pp_rec idsel n thy = 
  if idsel = Basic.fn_id 
  then thy.id_pps <- Lib.filter (fun (x, _) -> x=n )thy.id_pps
  else thy.type_pps <- Lib.filter (fun (x, _) -> x=n )thy.type_pps

let get_pplist idsel thy =
  let ppl = 
    if idsel = Basic.fn_id
    then thy.id_pps else thy.type_pps
  and tn = thy.name 
  in List.map (fun (x, y) -> (Basic.mk_long tn x , y)) ppl

let add_axiom n ax ps thy =
  if not (get_protection thy)
  then 
    if not (Lib.member n thy.axioms)
    then 
      let rcrd= { thm = ax; props = ps }
      in Hashtbl.add (thy.axioms) n rcrd
    else raise (Result.error ("Axiom "^n^" exists"))
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))

let add_thm n t ps thy =
  if not (get_protection thy)
  then 
    if not (Lib.member n thy.theorems)
    then
      let rcrd= { thm = t; props = ps }
      in Hashtbl.add (thy.theorems) n rcrd
    else raise (Result.error ("Theorem "^n^" exists"))
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))

let add_type_rec tr thy =
  let mk_typedef_rec n ags d cs =
    {Scope.name = n;
     Scope.args = ags;
     Scope.alias = d;
     Scope.characteristics = cs}
  in 
  let dest_tydef tydef= 
    if(Logic.Defns.is_typealias tydef)
    then 
      Logic.Defns.dest_typealias tydef
    else
      if(Logic.Defns.is_subtype tydef)
      then 
	let ctyrec=Logic.Defns.dest_subtype tydef
	in 
	(ctyrec.Logic.Defns.type_name, ctyrec.Logic.Defns.type_args, None)
      else
	raise 
	  (Result.error "Theory.add_type_rec: Expected a type definition")
  in 
  if not (get_protection thy)
  then 
    let (lid, args, df)=dest_tydef tr
    in 
    let id=Basic.name lid
    in 
    let tr=mk_typedef_rec id args df []
    in 
    if not (Lib.member id thy.typs)
    then Hashtbl.add (thy.typs) id tr
    else raise (Result.error ("Type "^id^" exists"))
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))


let get_type_rec n thy = Hashtbl.find (thy.typs) n

let get_defn_rec n thy = 
  let rcrd = (Hashtbl.find (thy.defns) n)
  in 
  {
   typ=Gtypes.rename_type_vars (rcrd.typ); 
   def=rcrd.def; 
   (* infix = rcrd.infix; prec=rcrd.prec; *)
   dprops = rcrd.dprops
 }
    

let get_defn n thy = 
  (let r = get_defn_rec n thy
  in match r.def with
    None -> raise (Result.error ("No definition for "^n))
  | Some(d) -> d)

let get_id_type n thy = 
  (let r = (get_defn_rec n thy)
  in r.typ)

let id_exists n thy =
  try 
    (ignore(get_defn_rec n thy); true)
  with Not_found -> false

let type_exists n thy =
  try 
    (ignore(get_type_rec n thy); true)
  with Not_found -> false

let add_defn_rec n ty d inf pr prop thy =
  if not (get_protection thy)
  then 
    (if id_exists n thy
    then raise (Result.error ("Identifier "^n^" already exists in theory"))
    else (Hashtbl.add (thy.defns) n 
	    {typ=ty; def=d;  
	     (* infix=inf; prec=pr;*)
	     dprops=prop}))
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))

let add_defn n ty d prop thy =
  add_defn_rec n ty (Some d) false (-1) prop thy

let set_defn_props n ps thy =
  if not (get_protection thy)
  then 
    let dr = get_defn_rec n thy
    in 
    let ndr= {dr with dprops=ps}
    in 
    Hashtbl.replace (thy.defns) n ndr
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))


let add_decln_rec n ty pr props thy =
  add_defn_rec n ty  None false pr props thy

let get_axiom_rec n thy = 
  try Hashtbl.find thy.axioms n
  with Not_found -> 
    raise (Result.error 
	     ("Axiom "^n^" not found in theory "^(get_name thy)^"."))

let get_theorem_rec n thy = 
  try Hashtbl.find thy.theorems n
  with Not_found -> 
    raise (Result.error 
	     ("Theorem "^n^" not found in theory "^(get_name thy)^"."))

let set_axiom_props n ps thy =
  if not (get_protection thy)
  then 
    let ar = get_axiom_rec n thy
    in 
    let nar= {ar with props=ps}
    in 
    Hashtbl.replace (thy.axioms) n nar
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))

let set_theorem_props n ps thy =
  if not (get_protection thy)
  then 
    let tr = get_theorem_rec n thy
    in 
    let ntr= {tr with props=ps}
    in 
    Hashtbl.replace (thy.theorems) n ntr
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))



let get_axiom n thy = 
  let ar = get_axiom_rec n thy
  in
  ar.thm

let get_theorem n thy = 
  let tr = get_theorem_rec n thy
  in
  tr.thm

let to_list tbl = 
  let tmp = ref []
  in 
  (Hashtbl.iter (fun x y -> tmp:=((x, y)::!tmp)) tbl); !tmp

let from_list xs = 
  let tenv = Hashtbl.create 1 
  in
  let rec from_list_aux ys=
    match ys with
      [] -> tenv
    | (x, y)::xss -> Hashtbl.add tenv x y; from_list_aux xss
  in ignore(from_list_aux xs); tenv

let to_save ir =
  {sty=ir.typ; 
   sdef = (match ir.def with 
     None -> None | Some(d) -> Some (Logic.to_save d));
(*   sinfix=ir.infix;  sprec=ir.prec; *)
   sdprops = ir.dprops}

let from_save sr =
  {typ=sr.sty; 
   def = (match sr.sdef with 
     None -> None | Some(d) -> Some(Logic.from_save d));
(*   infix=sr.sinfix;  prec=sr.sprec; *)
   dprops = sr.sdprops}

let thm_to_save tr=
  { sthm = Logic.to_save tr.thm; 
    sprops = tr.props }

let thm_from_save sr=
  { thm = Logic.from_save sr.sthm; 
    props = sr.sprops }

let output_theory oc thy = 
  let mk_save f xs = List.map (fun (x, y) -> (x, f y)) xs
  in 
  let saxs = mk_save thm_to_save (to_list thy.axioms)
  and sthms = mk_save thm_to_save (to_list thy.theorems)
  and sdefs = mk_save to_save (to_list thy.defns)
  and stypes = mk_save Gtypes.to_save_rec (to_list thy.typs)
  and styp_pps = thy.type_pps
  and sid_pps = thy.id_pps
  in output_value oc 
    (thy.name, thy.protection, thy.date, thy.parents, thy.lfiles,
     saxs, sthms, sdefs, stypes, styp_pps, sid_pps)

let input_theory ic = 
  let unsave f xs = from_list (List.map (fun (x, y) -> (x, f y)) xs)
  and n, prot, tim, prnts, lfls, saxs, sthms, 
    sdefs, stypes, ntype_pps, nid_pps = input_value ic 
  in 
  let axs = unsave thm_from_save saxs
  and thms = unsave thm_from_save sthms
  and defs = unsave from_save sdefs
  and tydefs = unsave Gtypes.from_save_rec stypes
  in 
  {name=n; protection=prot; date=tim; parents=prnts; lfiles = lfls;
   axioms = axs; theorems = thms; defns= defs; typs=tydefs;
   type_pps = ntype_pps; id_pps = nid_pps}

let load_theory fname = 
  let ic = open_in fname
  in let thy = input_theory ic; 
  in close_in ic; 
  Format.printf "@[Loading theory %s@]@." (get_name thy);
  thy

let save_theory thy prot fname= 
  if not (get_protection thy)
  then 
    (let oc = open_out fname
    in if prot then (set_protection thy) else ();
    output_theory oc thy; 
    close_out oc)
  else raise (Result.error ("Theory "^(get_name thy)^" is protected"))


let end_theory thy prot = 
  if not (get_protection thy)
  then 
    (if prot 
    then (set_date thy; set_protection thy)
    else ())
  else ()

let export_theory oc thy prot =
  end_theory thy prot;
  output_theory oc thy

let contents thy = 
  let to_list tbl =
    let l=ref [] 
    in 
    (Hashtbl.iter (fun n x -> l:=((n, x)::!l)) tbl); List.rev (!l)
  in 
  {
   cname = thy.name;
   cprotection = thy.protection;
   cdate = thy.date;
   cparents = thy.parents;
   cfiles = thy.lfiles;
   caxioms = to_list thy.axioms;
   ctheorems = to_list thy.theorems;
   cdefns = to_list thy.defns;
   ctyps = to_list thy.typs;
   ctype_pps = thy.type_pps;
   cid_pps = thy.id_pps
 }


(*
let print_property pp p = 
  match p with
    SimpProperty -> (Format.printf "@[simp@]")
  | _ -> (Format.printf "@[%s@]" (Printexc.to_string p))
*)
let print_property pp p = 
  Format.printf "@[%s@]" p

let print_properties pp ps =
  match ps with
    [] -> ()
  | _ -> 
      (Format.printf "@[(";
       Printer.print_list
	 ((fun p -> print_property pp p),
	  (fun _ -> Format.printf ",@ ")) ps;
       Format.printf ")@]@,")

let print_section title = 
  Format.printf "@[-----\n%s\n-----@]@," title

let print_protection p = 
  if(p) then ()
  else Format.printf "@[read-write@]@,"
and print_date d =
  let (y, mo, day, h, mi) = Lib.nice_date d
  in 
  Format.printf "@[Date: %i/%i/%i %i:%i@]@," day (mo+1) y h mi
and print_parents ps = 
  Format.printf "@[<2>Parents: ";
  (match ps with
    [] -> (Format.printf "%s" "None")
  | _ -> 
      Printer.print_list 
	((fun s -> Format.printf "%s" s),
	 (fun _ -> Format.printf "@ ")) ps);
  Format.printf "@]@,"
and print_files ps = 
  Format.printf "@[<2>Load Files: ";
  (match ps with
    [] -> (Format.printf "None")
  | _ -> 
      Printer.print_list 
	((fun s -> Format.printf "%s" s),
	 (fun _ -> Format.printf "@ ")) ps);
  Format.printf "@]@,"
and print_thms pp n ths = 
  print_section n;
  Format.printf "@[<v>";
  Printer.print_list
    ((fun (tn, t) ->
      Format.printf "@[<2>%s:@ " tn;
      print_properties pp t.props;
      Format.printf "@ ";
      Logic.print_thm pp t.thm;
      Format.printf "@]"),
     (fun _ -> ())) ths;
  Format.printf "@]@,"
and print_tydefs pp n tys = 
  print_section n;
  Format.printf "@[<v>";
  Printer.print_list
    ((fun (n, tyd) ->
      Format.printf "@[<2>";
      (match tyd.Scope.args with
	[] -> ()
      | _ -> 
	  (Format.printf "(";
	   Printer.print_list
	     ((fun s -> Format.printf "'%s" s),
	      (fun _ -> Format.printf ",@ "))
	     tyd.Scope.args;
	   Format.printf ")"));
      Format.printf "%s@," n;
      (match tyd.Scope.alias with
	None -> ()
      | Some(gty) -> 
     	  (Format.printf "=@,";
	  Gtypes.print pp gty));
      Format.printf "@]"),
     (fun _ -> ())) tys;
  Format.printf "@]@,"
and print_defs pp n defs = 
  print_section n;
  Format.printf "@[<v>";
  Printer.print_list
    ((fun (n, d) ->
      Format.printf "@[<2>%s:@ " n;
      print_properties pp d.dprops;
      Format.printf "@ ";
      Gtypes.print pp d.typ;
      Format.printf "@ ";
      (match d.def with
	None -> ()
      | Some(df) ->
	  Logic.print_thm pp df);
      Format.printf "@]"),
     (fun _ -> ())) defs;
  Format.printf "@]@,"

let print_pps n pps = 
  print_section n;
  Format.printf "@[<v>";
  Printer.print_list
    ((fun (n, r) ->
      Format.printf "@[<2>%s@ " n;
      (match (r.Printer.repr) with
	None -> ()
      | Some(s) -> Format.printf "\"%s\"@ " s);
      Format.printf "precedence= %i@ " r.Printer.prec;
      Format.printf "fixity= %s@ "
	(Printer.fixity_to_string r.Printer.fixity);
      Format.printf "@]"),
     (fun _ -> ())) pps;
  Format.printf "@]@,"

let print ppstate thy = 
  let content = contents thy 
  in 
  Format.printf "@[<v>";
  Format.printf "@[-------------@]@,";
  Format.printf "@[%s@]@," content.cname;
  print_parents content.cparents;
  print_files content.cfiles;
  print_date content.cdate;
  print_protection content.cprotection;
  print_tydefs ppstate "Types" content.ctyps;
  print_thms ppstate "Axioms" content.caxioms;
  print_defs ppstate "Definitions" content.cdefns;
  print_thms ppstate "Theorems" content.ctheorems;
  (match content.ctype_pps with 
    [] -> () 
  | _ -> 
      print_pps "Type printer/parser information" content.ctype_pps);
  (match content.cid_pps with 
    [] -> ()
  | _ -> 
      print_pps "Term printer/parser information" content.cid_pps);
  Format.printf "@[-------------@]@,";
  Format.printf "@]"
    
