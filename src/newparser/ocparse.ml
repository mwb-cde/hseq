(* term/type parsing with camlp4 *)


module Ocparse=
struct

open Logicterm

  type parse_info =
      {
       bound_vars : (string * Term.term) list;
       type_vars : (string * Gtypes.gtype) list
(*       lexer_info: Oclex.symtable*)
     }
	
  let new_parse_info lexinf = 
    { bound_vars= []; type_vars = [] ; 
      (*lexer_info = lexinf*)
    }
      
(*
   push a type/term name on the info stack 
   with their assoicated type/term
*)

  let info_push_term inf n t =
    {
     bound_vars= (n, t)::inf.bound_vars;
     type_vars=inf.type_vars
    }

  let info_push_type inf n t =
    {
     bound_vars= inf.bound_vars;
     type_vars=(n, t)::inf.type_vars
    }

(* find/remove types and terms associated with a name *)

  let info_find_term inf n =
     List.assoc n inf.bound_vars

  let info_find_type inf n =
     List.find n inf.type_vars

  let info_remove_term inf n=
    List.remove_assoc n (inf.bound_vars)

  let info_remove_type inf n=
    List.remove_assoc n (inf.type_vars)

      (* info_drop_* : drop the topmost term/type in the list *)
      
  let info_drop_term inf =
    {
     bound_vars= List.tl(inf.bound_vars);
     type_vars=inf.type_vars
(*     lexer_info=inf.lexer_info*)
    }

  let info_drop_type inf =
    {
     bound_vars= inf.bound_vars;
     type_vars=List.tl(inf.type_vars)
(*     lexer_info=inf.lexer_info*)
    }
    
(* utility function s*)

let mk_id str = Lib.chop_at '.' str

let get_id (_, s)= s

(* parsers *)

let gram = 
  Grammar.create Oclexer.ocp4lex

let term =
  Grammar.Entry.create gram "term"

(* simple terms *)

EXTEND
term:
[ "quantifier" LEFTA
  [ LAM; x=ID ; COLON; y=term ->
    fun env -> 
    (let id=mk_id x
    in 
    let qnt=Term.mk_binding Basic.Lambda (Basic.name id) (Gtypes.mk_null())
    in 
    let ret=
      env:=info_push_term (!env) x (Term.Bound(qnt));
      Term.Qnt(qnt, (y env))
    in 
    env:=info_drop_term (!env); ret)]
| 
  [ EXISTS; x=ID ; COLON; y=term ->
    fun env -> 
    (let id=mk_id x
    in 
    let qnt=Term.mk_binding Basic.Ex (Basic.name id) (Gtypes.mk_null())
    in 
    let ret=
      env:=info_push_term (!env) x (Term.Bound(qnt));
      Term.Qnt(qnt, (y env))
    in 
    env:=info_drop_term (!env); ret)]
| 
  [ ALL; x=ID ; COLON; y=term ->
    fun env -> 
    (let id=mk_id x
    in 
    let qnt=Term.mk_binding Basic.All (Basic.name id) (Gtypes.mk_null())
    in 
    let ret=
      env:=info_push_term (!env) x (Term.Bound(qnt));
      Term.Qnt(qnt, (y env))
    in 
    env:=info_drop_term (!env); ret)]
| "application" LEFTA
  [ x=term ; y = term -> fun env -> Term.mkapp (x env) (y env)]
| "equals" RIGHTA
  [ x=term; EQUALS; y=term -> fun env -> mkequal (x env) (y env) ]
| "implies" RIGHTA
  [ x=term; IMPLIES; y=term -> fun env -> mkimplies (x env) (y env)
|   x=term; IFF; y=term -> fun env -> mkiff (x env) (y env) ]
| "binary_op" RIGHTA
  [ x=term; AND; y=term -> fun env -> mkand (x env) (y env)
|   x=term; OR; y=term -> fun env -> mkor (x env) (y env) ]
| "unary_op" RIGHTA
  [ NOT; x=term -> fun env -> mknot (x env) ] 
| "constant" NONA
  [ 
  TRUE -> fun env -> Term.mkbool true 
| FALSE -> fun env -> Term.mkbool false
| x=ID  -> fun env -> 
    (let id=mk_id x
    in 
    try 
      info_find_term (!env) x
    with Not_found -> Term.mkvar(mk_id x))
  ]
| [ ORB; x=term ; CRB -> fun env -> (x env)]];
  END

(*
EXTEND 
term: BEFORE "0"
[
  [ x=ID -> (fun env -> Term.mkbool true) ] 
];
END
*)
    
(* toplevel term and type parsers *)

let term_parser str=
 Grammar.Entry.parse 
    term
    (Stream.of_string str) (ref(new_parse_info()))

let type_parser = ()

end

