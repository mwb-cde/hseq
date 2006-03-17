(*-----
 Name: simplib.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

open Tactics

let std_simpset = ref(Simpset.empty_set())

let std_ss() = !std_simpset
let set_std_ss s = std_simpset:=s
let empty_simp () = set_std_ss (Simpset.empty_set())
let add_simps thms = 
  set_std_ss (Simpset.simpset_add_thms (Global.scope()) (std_ss()) thms)
let add_simp thm = add_simps [thm]

let add_conv terms conv =
  let add_aux set trm = 
    let (vs, body) = Term.strip_qnt Basic.All trm
    in 
      Simpset.add_conv (vs, body) conv set
  in 
  let set1 =
    List.fold_left add_aux (std_ss()) terms
  in 
    set_std_ss set1

let init_std_ss() =
  empty_simp();
  add_conv [<< !x A: (%y: A) x >>] Logic.Conv.beta_conv

(***
* Toplevel simplification tactics
***)

let simpC_tac 
    ?(cntrl=Formula.default_rr_control) ?(ignore = [])
    ?set ?add ?c rules goal =
(** uset: The simpset to use. **)
  let scp = scope_of goal
  in 
  let uset = 
    let uset0 = 
      match set with
	None -> std_ss()
      | Some s -> s
    in 
    let uset1 = 
      match add with
	None -> uset0
      | Some s -> Simpset.join s uset0
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      [] -> uset1
    | _ -> 
	Simpset.simpset_add_thms scp uset1 rules
(*
	let rset = 
	  Simpset.simpset_add_thms 
	    (Global.scope()) (Simpset.empty_set()) rules
	in 
	Simpset.join rset uset1
*)
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = 
      Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.simpC_tac simp_data ?c goal

let simpC ?c goal = simpC_tac ?c [] goal

let simpA_tac 
    ?(cntrl=Formula.default_rr_control) ?(ignore = [])
    ?set ?add ?a rules goal =
(** uset: The simpset to use. **)
  let scp = scope_of goal
  in 
  let uset = 
    let uset0 = 
      match set with
	None -> std_ss()
      | Some s -> s
    in 
    let uset1 = 
      match add with
	None -> uset0
      | Some s -> Simpset.join s uset0
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      [] -> uset1
    | _ -> 
	Simpset.simpset_add_thms scp uset1 rules
(*
	let rset = 
	  Simpset.simpset_add_thms 
	    (Global.scope()) (Simpset.empty_set()) rules
	in 
	Simpset.join rset uset1
*)
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = 
      Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.simpA_tac simp_data ?a goal

let simpA ?a goal = simpA_tac ?a [] goal

let simp_all_tac 
    ?(cntrl=Formula.default_rr_control) ?(ignore = [])
    ?set ?add rules goal =
(** uset: The simpset to use. **)
  let scp = scope_of goal
  in 
  let uset = 
    let uset0 = 
      match set with
	None -> std_ss()
      | Some s -> s
    in 
    let uset1 = 
      match add with
	None -> uset0
      | Some s -> Simpset.join s uset0
    in 
    (** If there are rules, make a simpset from them. **)
    match rules with 
      [] -> uset1
    | _ -> 
	Simpset.simpset_add_thms scp uset1 rules
(*
	let rset = 
	  Simpset.simpset_add_thms 
	    (Global.scope()) (Simpset.empty_set()) rules
	in 
	Simpset.join rset uset1
*)
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    let data1 = 
      Simplifier.Data.set_exclude Simptacs.default_data ignore_tags
    in 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control data1 cntrl)
      uset
  in 
  Simptacs.full_simp_tac simp_data goal

let simp_all goal = simp_all_tac [] goal

let simp_tac 
    ?(cntrl=Formula.default_rr_control) ?(ignore = [])
    ?set ?add ?f rules goal =
  let sqnt = Tactics.sequent goal 
  in 
  let tac = 
    match f with 
      None -> simpC_tac ~cntrl:cntrl ~ignore:ignore ?set ?add rules
    | Some(x) -> 
	let tg = Logic.label_to_tag x sqnt
	in 
	(match 
	  Lib.try_find (get_tagged_concl (ftag tg)) goal 
	with
	  None -> 
	    simpA_tac 
	      ~cntrl:cntrl ~ignore:ignore ?set ?add ~a:(ftag tg)rules
	| _ -> 
	    simpC_tac
	      ~cntrl:cntrl ~ignore:ignore ?set ?add ~c:(ftag tg)rules)
  in 
  tac goal
	
let simp ?f goal = simp_tac ?f [] goal

(***
* Initialising functions 
***)


let has_property p ps = List.mem p ps

let thm_is_simp (_, tr)=
  if(has_property Theory.simp_property tr.Theory.props)
  then try (add_simp tr.Theory.thm) with _ -> ()
  else ()

let def_is_simp (_, dr)=
  match dr.Theory.def with
    None -> ()
  | Some(thm) -> 
      if(has_property Theory.simp_property dr.Theory.dprops)
      then try add_simp thm with _ -> ()
      else ()
  
(** Function to call when a theory is loaded **)

let on_load thy=
  List.iter thm_is_simp thy.Theory.caxioms;
  List.iter thm_is_simp thy.Theory.ctheorems;
  List.iter def_is_simp thy.Theory.cdefns


(** Initialise the simp set. **)
let init () =
  init_std_ss();
  Global.Files.add_load_fn on_load

let _ = 
  Global.Init.add_init init
  

(***
* Printer 
***)

let print_set set = 
  Simpset.print (Global.PP.info()) set

