(*-----
 Name: userlib.ml
 Author: M Wahab <mwahab@users.sourceforge.net>
 Copyright M Wahab 2005
----*)


(* Infixes *)

type fixity = Commands.fixity
let nonfix = Commands.nonfix
let prefix = Commands.prefix
let suffix = Commands.suffix
let infixl = Commands.infixl
let infixr = Commands.infixr
let infixn = Commands.infixn

(* error handling *)
let catch_errors = Commands.catch_errors

let theory = Commands.theory

let save_theory = Commands.save_theory
let load_theory = Commands.load_theory

let begin_theory = Commands.begin_theory
let open_theory = Commands.open_theory
let close_theory = Commands.close_theory
let end_theory = Commands.end_theory

let new_type=Commands.new_type

let define ?pp ?(simp=false) df =
  let ret = Commands.define ?pp ~simp:simp df
  in 
  if simp
  then 
    (let (_, _, thm) = Logic.Defns.dest_termdef ret
    in 
    Simplib.add_simp thm; ret)
  else ret

let declare = Commands.declare

let new_axiom ?(simp=false) n t =
  let thm = Commands.new_axiom ~simp:simp n t
  in 
  if(simp)
  then (Simplib.add_simp thm; thm)
  else thm

let axiom = Commands.axiom
let theorem = Commands.theorem
let defn = Commands.defn
let lemma = Commands.lemma
let parents = Commands.parents
let add_file = Commands.add_file
let remove_file = Commands.remove_file
let qed = Commands.qed

let prove_thm ?(simp=false) n t tac =
  let thm = Commands.prove_thm ~simp:simp n t tac
  in 
  if simp 
  then (Simplib.add_simp thm; thm)
  else thm

let save_thm ?(simp=false) n thm =
  let ret = Commands.save_thm ~simp:simp n thm
  in 
  if simp 
  then (Simplib.add_simp ret; ret)
  else ret

let scope = Commands.scope

let by x = 
  (catch_errors Goals.by_com) x
