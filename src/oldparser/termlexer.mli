module Lex:
sig
   type tok = Termlex.tok
   val lex : string -> tok list
   val add_keyword : string -> tok -> unit
   val remove_keyword : string -> unit
   val add_symbol : string -> tok -> unit
   val remove_symbol : string -> unit    
end


