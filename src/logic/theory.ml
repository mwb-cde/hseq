
(*
   exception Error of string
 *)


type id_record= {typ: Basic.gtype; def: Logic.thm option; 
		 infix: bool; prec: int}

type save_record= {sty: Basic.gtype; sdef: Logic.saved_thm option; 
		   sinfix: bool; sprec: int}

type thy = 
    {
     name: string;
     mutable protection: bool;
     mutable date: float;
     mutable parents:  string list;
     axioms: (string, Logic.thm) Hashtbl.t;
     theorems: (string, Logic.thm) Hashtbl.t;
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
     caxioms: (string * Logic.thm) list;
     ctheorems: (string * Logic.thm) list;
     cdefns: (string * id_record) list;
     ctyps: (string * Gtypes.typedef_record) list;
     ctype_pps: (string * Printer.record) list;
     cid_pps: (string * Printer.record) list 
   }

let mk_thy n = {name=n; 
		protection=false;
		date=0.0;
		parents=[];
		axioms=Hashtbl.create 1;
		theorems=Hashtbl.create 1;
		defns=Hashtbl.create 1; 
		typs = Hashtbl.create 1;
		type_pps = [];
		id_pps = []
	      }

let get_name thy = thy.name
let get_parents thy = thy.parents
let add_parent n thy =
  if (not thy.protection) & (n<>"") & (not (List.mem n (thy.parents)))
  then thy.parents<-(n::(thy.parents))
  else (Result.raiseError ("add_parent: "^n))

let add_parents ns thy =
  match ns with
    [] -> ()
  |	(x::xs) -> add_parent x thy


let get_date thy = thy.date
let set_date thy = thy.date<-Lib.date()

let get_protection thy = thy.protection
let set_protection thy = 
  thy.protection<-true;
  set_date thy


let add_pp_rec idsel n ppr thy=
  if not (get_protection thy)
  then 
    if idsel = Basic.fn_id 
    then 
      (if Lib.member n thy.defns
      then thy.id_pps <- ((n, ppr)::thy.id_pps)
      else (Result.raiseError 
	      ("No name "^n^" defined in theory "^(get_name thy))))
    else 
      (if Lib.member n thy.typs
      then thy.type_pps <- ((n, ppr)::thy.type_pps)
      else (Result.raiseError 
	      ("No type "^n^" defined in theory "^(get_name thy))))
  else (Result.raiseError ("Theory "^(get_name thy)^" is protected"))

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
  in List.map (fun (x, y) -> (Basic.mklong tn x , y)) ppl

let add_axiom n ax thy =
  if not (get_protection thy)
  then 
    if not (Lib.member n thy.axioms)
    then Hashtbl.add (thy.axioms) n ax
    else Result.raiseError ("Axiom "^n^" exists")
  else Result.raiseError ("Theory "^(get_name thy)^" is protected")

let add_thm n t thy =
  if not (get_protection thy)
  then 
    if not (Lib.member n thy.theorems)
    then Hashtbl.add (thy.theorems) n t
    else Result.raiseError ("Theorem "^n^" exists")
  else Result.raiseError ("Theory "^(get_name thy)^" is protected")

(*
   let add_type_rec n tr thy =
   if not (get_protection thy)
   then 
   if not (Lib.member n thy.typs)
   then Hashtbl.add (thy.typs) n tr
   else Result.raiseError ("Type "^n^" exists")
   else Result.raiseError ("Theory "^(get_name thy)^" is protected")
 *)

let add_type_rec tr thy =
  let mk_typedef_rec n ags d cs =
    {Gtypes.name = n;
     Gtypes.args = ags;
     Gtypes.alias = d;
     Gtypes.characteristics = cs}
  in 
  if not (get_protection thy)
  then 
    let (lid, args, df)=Logic.Defns.dest_typedef tr
    in 
    let id=Basic.name lid
    in 
    let tr=mk_typedef_rec id args df []
    in 
    if not (Lib.member id thy.typs)
    then Hashtbl.add (thy.typs) id tr
    else Result.raiseError ("Type "^id^" exists")
  else Result.raiseError ("Theory "^(get_name thy)^" is protected")


let get_type_rec n thy = Hashtbl.find (thy.typs) n

let get_defn_rec n thy = 
  let rcrd = (Hashtbl.find (thy.defns) n)
  in {typ=Gtypes.copy_type (rcrd.typ); 
      def=rcrd.def; infix = rcrd.infix; prec=rcrd.prec}
    

let get_defn n thy = 
  (let r = get_defn_rec n thy
  in match r.def with
    None -> Result.raiseError ("No definition for "^n)
  | Some(d) -> d)

let get_id_type n thy = 
  (let r = (get_defn_rec n thy)
  in r.typ)

let id_is_infix n thy = 
  (let r =  get_defn_rec n thy
  in r.infix)

let get_id_prec n thy = 
  (let r =  get_defn_rec n thy
  in r.prec)

let id_exists n thy =
  try 
    (ignore(get_defn_rec n thy); true)
  with Not_found -> false

let add_defn_rec n ty d inf pr thy =
  if not (get_protection thy)
  then 
    (if id_exists n thy
    then Result.raiseError ("Identifier "^n^" already exists in theory")
    else (Hashtbl.add (thy.defns) n 
	    {typ=ty; def=d;  infix=inf; prec=pr}))
  else Result.raiseError ("Theory "^(get_name thy)^" is protected")

let add_defn n ty d thy =
  add_defn_rec n ty (Some d) false (-1) thy

let add_decln_rec n ty pr thy =
  add_defn_rec n ty  None false pr thy

let get_axiom n thy = 
  try Hashtbl.find thy.axioms n
  with Not_found -> 
    Result.raiseError 
      ("Axiom "^n^" not found in theory "^(get_name thy)^".")

let get_theorem n thy = 
  try Hashtbl.find thy.theorems n
  with Not_found -> 
    Result.raiseError 
      ("Theorem "^n^" not found in theory "^(get_name thy)^".")

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
   sinfix=ir.infix;
   sprec=ir.prec}

let from_save sr =
  {typ=sr.sty; 
   def = (match sr.sdef with 
     None -> None | Some(d) -> Some(Logic.from_save d));
   infix=sr.sinfix;
   prec=sr.sprec}


let output_theory oc thy = 
  let mksave f xs = List.map (fun (x, y) -> (x, f y)) xs
  in 
  let saxs = mksave Logic.to_save (to_list thy.axioms)
  and sthms = mksave Logic.to_save (to_list thy.theorems)
  and sdefs = mksave to_save (to_list thy.defns)
  and stypes = mksave Gtypes.to_save_rec (to_list thy.typs)
  and styp_pps = thy.type_pps
  and sid_pps = thy.id_pps
  in output_value oc 
    (thy.name, thy.protection, thy.date, thy.parents, 
     saxs, sthms, sdefs, stypes, styp_pps, sid_pps)

let input_theory ic = 
  let unsave f xs = from_list (List.map (fun (x, y) -> (x, f y)) xs)
  and n, prot, tim, prnts, saxs, sthms, 
    sdefs, stypes, ntype_pps, nid_pps = input_value ic 
  in 
  let axs = unsave Logic.from_save saxs
  and thms = unsave Logic.from_save sthms
  and defs = unsave from_save sdefs
  and tydefs = unsave Gtypes.from_save_rec stypes
  in 
  {name=n; protection=prot; date=tim; parents=prnts; 
   axioms = axs; theorems = thms; defns= defs; typs=tydefs;
   type_pps = ntype_pps; id_pps = nid_pps}

let load_theory fname = 
  let ic = open_in fname
  in let thy = input_theory ic; 
  in close_in ic; 
  print_string ("Loading theory "^(get_name thy)^"\n");
  thy

let save_theory thy prot fname= 
  if not (get_protection thy)
  then 
    (let oc = open_out fname
    in if prot then (set_protection thy) else ();
    output_theory oc thy; 
    close_out oc)
  else Result.raiseError ("Theory "^(get_name thy)^" is protected")


let export_theory oc thy prot =
  if prot then set_protection thy else set_date thy;
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
   caxioms = to_list thy.axioms;
   ctheorems = to_list thy.theorems;
   cdefns = to_list thy.defns;
   ctyps = to_list thy.typs;
   ctype_pps = thy.type_pps;
   cid_pps = thy.id_pps
 }

let print_section title = 
  Format.print_string "-----";
  Format.print_newline();
  Format.print_string title;
  Format.print_newline();
  Format.print_string "-----";    
  Format.print_newline()

let print_protection p = 
  if(p)
  then ()
  else print_string "read-write"
and print_date d =
  let (y, mo, day, h, mi) = Lib.nice_date d
  in 
  Format.open_box 0;
  Format.print_string "Date: ";
  Format.print_int day;
  Format.print_string "/";
  Format.print_int mo;
  Format.print_string "/";
  Format.print_int y;
  Format.print_string " ";
  Format.print_int h;
  Format.print_string ":";
  Format.print_int mi;
  Format.close_box()
and print_parents ps = 
  Format.print_string "Parents: ";
  (match ps with
    [] -> (Format.print_string "None"; 
	   Format.print_newline())
  | _ -> 
      Format.open_box 2;
      Printer.print_list 
	(Format.print_string,
	 (fun _ -> Format.print_space())) ps;
      Format.close_box());
and print_thms pp n ths = 
  print_section n;
  Printer.print_list
    ((fun (tn, t) ->
      Format.open_box 4;
      Format.print_string tn;
      Format.print_string ": ";
      Format.print_newline();
      Logic.print_thm pp t;
      Format.print_newline();
      Format.close_box()),
     (fun _ -> Format.print_newline())) ths

and print_tydefs pp n tys = 
  print_section n;
  Format.open_box 0;
  Printer.print_list
    ((fun (n, tyd) ->
      Format.open_box 4;
      Format.print_string n;
      (match tyd.Gtypes.args with
	[] -> ()
      | _ -> 
	  Format.open_box 1;
	  Format.print_string "(";
	  Printer.print_list
	    ((fun s -> Format.print_string s),
	    (fun _ -> Format.print_string ","; Format.print_space()))
	    tyd.Gtypes.args;
	  Format.close_box());
      (match tyd.Gtypes.alias with
	None -> ()
      | Some(gty) -> 
	  Format.open_box 1;
	  Format.print_string "=";
	  Format.print_space ();
	  Gtypes.print pp gty;
	  Format.print_newline();
	  Format.close_box());
      Format.close_box()),
    (fun _ -> Format.print_newline()))
    tys;
  Format.close_box()

and print_defs pp n defs = 
  print_section n;
  Printer.print_list
    ((fun (n, d) ->
      Format.print_string n;
      Format.print_string ": ";
      Format.open_box 4;
      Gtypes.print pp d.typ;
      Format.print_newline();
      (match d.def with
	None -> ()
      | Some(df) -> 
	  Logic.print_thm pp df;
	  Format.print_newline ());
      Format.close_box()),
    (fun _ -> Format.print_newline()))
    defs

let print_pps n pps = 
  print_section n;
  Format.open_box 4;
  Printer.print_list
    ((fun (n, r) ->
      Format.open_box 2;
      Format.print_string n;
      Format.print_space();
      (match (r.Printer.repr) with
	None -> ()
      | Some(s) -> 
	  Format.print_string ("\""^s^"\"");
	  Format.print_space());
      Format.print_string "precedence=";
      Format.print_int r.Printer.prec;
      Format.print_space();
      Format.print_string "fixity=";
      Format.print_string (Printer.fixity_to_string r.Printer.fixity);
      Format.close_box()),
    (fun _ -> Format.print_newline()))
    pps;
  Format.close_box()

let print ppstate thy = 
  let content = contents thy 
  in 
  Format.open_box 0;
  Format.print_newline();
  Format.print_string "-------------";
  Format.print_newline();
  Format.print_string content.cname;
  Format.print_newline();  
  print_date content.cdate;
  Format.print_newline();  
  print_protection content.cprotection;
  Format.print_newline();
  print_parents content.cparents;
  Format.print_newline();
  print_tydefs ppstate "Types" content.ctyps;
  Format.print_newline();
  print_thms ppstate "Axioms" content.caxioms;
  Format.print_newline();
  print_defs ppstate "Definitions" content.cdefns;
  Format.print_newline();
  print_thms ppstate "Theorems" content.ctheorems;
  Format.print_newline();
  (match content.ctype_pps with 
    [] -> () 
  | _ -> 
      print_pps "Type printer/parser information" content.ctype_pps;
      Format.print_newline());
  (match content.cid_pps with 
    [] -> ()
  | _ -> 
      print_pps "Term printer/parser information" content.cid_pps);
  Format.print_newline();
  Format.print_string "-------------";
  Format.close_box()
