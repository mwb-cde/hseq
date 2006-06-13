(*-----
Name: setLib.mli
Author: M Wahab <mwahab@users.sourceforge.net>
Copyright M Wahab 2006
----*)

(*
   setLib.mli:

   Support code for setScript.ml.

   Adds a parser and printer for set terms.

   Parse and print [SET (%x: P)] as [{x: P}].

   Parse [{} ] as [empty].  The standard printer handles the empty set
   correctly.

   compile with [compile [] "setLib.mli"; compile [] "setLib.ml"]
*)

open HSeq

val set_thy: string
(**
   [set_thy]: The name of the theory of sets.
*)

val set_id : Ident.t
(**
   [set_id] The identifier of the set constructor
*)

val set_data:(Printer.fixity* int)
(**
   [set_data] The precedence and fixity of the set identifier 
*)

(** 
   [empty_id] The identifier of the set constructor

   [empty_data] PP data for the empty set.
*)
val empty_id : Ident.t
val empty_data:(Printer.fixity* int)


module SetPP:
    sig
      val ocb_sym: string
      val ccb_sym: string

      val set_parser: 
	  unit 
	-> (Grammars.parser_info -> Basic.term Parser.phrase)
      val set_printer : 
	  unit 
	-> Printer.ppinfo -> (Printer.fixity * int)
	  -> (Basic.term * Basic.term list) Printer.printer

end
