(* lexer for logic *)

    type symbols =
      | DOT
      | COMMA
      | ORB
      | CRB
      | RIGHTARROW
      | PRIME
      | COLON
      | OTHER of string
    and keys = | ALL | EX | LAM (*| AND | OR | NOT | IMPLIES | EQUALS | IFF*)
    and tok =
      | Key of keys
      | Sym of symbols
      | ID of Basic.fnident
      | NUM of string
      | BOOL of bool

exception Error
    val all_qnts : (string * tok) list
    val ex_qnts : (string * tok) list
    val lam_qnts : (string * tok) list
    val not_ops : (string * tok) list
    val and_ops : (string * tok) list
    val or_ops : (string * tok) list
    val implies_ops : (string * tok) list
    val iff_ops : (string * tok) list
    val equals_ops : (string * tok) list
    val keywords_list : (string * tok) list ref
    val keywords : unit -> (string * tok) list
    val remove : 'a -> ('a * 'b) list -> ('a * 'b) list
    val token_of : 'a -> ('a * 'b) list -> 'b
    val add_to_list : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
    val add_keyword : string -> tok -> unit
    val remove_keyword : string -> unit
    val white_space : char list
      val special : char list
    val syms_list : (string * tok) list ref
    val syms : unit -> (string * tok) list
    val add_symbol : string -> tok -> unit
    val remove_symbol : string -> unit
    val is_num : string -> int -> bool
    val first : (char -> bool) -> string -> int -> int
    val get_num : string -> int -> tok * int
    val other_lex : (string -> int -> tok * int) ref
    val set_other : (string -> int -> tok * int) -> unit
    val other : unit -> string -> int -> tok * int
    val mk_id : string -> tok
    val string_of_token : tok -> string
    val print_token : tok -> unit
  
