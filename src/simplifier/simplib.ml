(*-----
 Name: simplib.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)

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

(** [simp_tac ?f ?cntrl ?asms ?set ?with ?rules ?ignore goal]
   
   Simplifier tactic.

   If [f] is not given, repeat for each conclusion:
   - eliminate toplevel universal quantifiers of [f]
   - if (asms=true), put conclusions other than [f] into assumptions
   and make simp rules
   - if (asms=true), make simp rules from assumptions, other than [f]
   - simplify [f]: find (possibly conditional) rules for
   rewriting [f], rewrite [f], repeat until no change.

   When done, delete temporary assumptions

   Don't use formulas in [ignore] or for which [except] is true.

   Arguments:

   @param f The formula to simplify. Default: all conclusions.

   @param cntrl The rewrite control to use (used to select top-down or
   bottom up simplifying). Default: top-down.  

   @param asms Whether to use the assumptions and conclusions as
   rewrite rules. Default: true.

   @params set The simpset to use. Default: [std_ss].

   @params use Add this simpset to the set specified with [set]. This
   allows extra simpsets to be used with the standard simpset.

   @param rules Additional rewrite rules to use. Default: [].

   @param ignore List of assumptions/conclusions to ignore. Default: [].

   @raise Simplifier.No_change If no change is made.
 *)
let simp_tac 
    ?f ?(cntrl=Formula.default_rr_control) 
    ?(asms=true) ?set ?add ?(rules=[]) ?(ignore = []) goal =
(** uset: The simpset to use. **)
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
	let s = 
	  Simpset.simpset_add_thms 
	    (Global.scope()) (Simpset.empty_set()) rules
	in 
	Simpset.join s uset1
  in 
  (** ignore_tags: The tags of sequent formulas to be left alone. **)
  let ignore_tags = 
    let sqnt = Tactics.sequent goal 
    in 
    List.map (fun l -> Logic.label_to_tag l sqnt) ignore
  in 
  (** except: The test for an excluded formula. **)
  let except x = List.exists (Tag.equal x) ignore_tags
  in 
  (** The simplifier arguments. **)
  let args = Simptacs.mk_args asms except
  in 
  (** simp_data: The simpset data. *)
  let simp_data = 
    Simplifier.Data.set_simpset
      (Simplifier.Data.set_control Simplifier.Data.default cntrl)
      uset
  in 
  Simptacs.simp_tac simp_data args f goal

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

