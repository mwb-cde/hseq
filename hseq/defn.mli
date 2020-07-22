(*----
  Copyright (c) 2005-2021 Matthew Wahab <mwb.cde@gmail.com>

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
----*)

(** Term and subtype definition *)

(** {5 Term definition and declaration} *)

(** {7 Term declaration} *)

val mk_decln:
  Scope.t ->
  Ident.t -> Gtype.t -> (Ident.t * Gtype.t)
(** Declare term identifier [(f:ty)]. *)

(** {7 Term definition} *)

val mk_defn:
  Scope.t
  -> (Ident.t * Gtype.t)
  -> Term.term list
  -> Term.term
  -> (Ident.t * Gtype.t * Formula.t)
(** [mk_defn scp id args t]: Construct a definition.

    [scp] is the scope of the definition
    [id] is the identifier being defined.
    [args] is the list of arguments, where each argument is a
    (universally) bound variable.
    [t] is the body of the definition

    Returns a formula of the form [! id args: id args = t] together
    with the long identifier and type of the defined term.
*)

(** {5 Type definition} *)

(** {7 Support functions} *)

val check_args_unique: string list -> unit
(** [check_args_unqiues args]: Ensure that all argument names in
    [args] are unique.
*)

val check_type_name: Scope.t -> Ident.t -> unit
(** [check_type_name scp n]: Make sure that there is no type named [n]
    in scope [scp].
*)

val check_well_defined: Scope.t -> string list -> Gtype.t -> unit
(** [check_well_defined scp args ty]: Make sure that type [ty] is well
    defined in scope [scp]. Every type variable in [ty] must have
    a name in [args], weak variables are not permitted and every
    constructor must be declared in [scp].
*)

(** {7 Subtype definition}

    Define the subtype of a type constructing
    [A, args, T, set:(args)T->bool, rep, abs]
    where
    {ul
    {- [A] is the name of the subtype.}

    {- [args] are the names of the types' parameters.}

    {- [T] is the representation type.}

    {- [rep] is the name of the representation function
    destructing type [A]}

    {- [abs] is the name of the abstraction function constructing
    type [A].}}

    The types of the constructed functions are
    {ul
    {- representation function:  [rep:(args)T -> A]}

    {- abstraction function: [abs:A-> (args)T]}}

    The axioms specifying the abstraction and representation functions are:
    {ul

    {- rep_T: |- !x: set (rep x)}

    {- rep_T_inverse: |- !x: abs (rep x) = x}

    {- abs_T_inverse: |- !x: (set x) => rep (abs x) = x}}

    The names of the type, abstraction and representation functions are
    all parameters to the subtype definition.
*)

type subtype_defn =
    {
      id: Ident.t;
      args: string list;
      rep: (Ident.t * Gtype.t);
      abs: (Ident.t * Gtype.t);
      set: Term.term;
      rep_T: Term.term;
      rep_T_inverse: Term.term;
      abs_T_inverse: Term.term
    }
(**
    The result of constructing a subtype:
    {ul
    {- [id]: The name of the new type.}
    {- [args]: The names of the parameters to the type.}
    {- [rep]: The declaration of the representation function.}
    {- [abs]: The declaration of the abstraction function.}
    {- [set]: The term providing the defining predicate of the subtype.}
    {- [rep_T]: A term of the form |- !x: set (rep x)}
    {- [rep_T_inverse]: A term of the form |- !x: abs (rep x) = x}
    {- [abs_T_inverse]: A term of the form |- !x: (set x) => rep
    (abs x) = x}}
*)


val mk_subtype_exists: Term.term -> Term.term
(** [mk_subtype_exists setp]: Make the term << ?x: setp x >> to be
    used to show that the subtype is not empty.
*)

val make_witness_type:
  Scope.t -> Gtype.t -> Term.term -> Term.term
(** [make_witness_type scp ty setp]: Make a witness to the
    non-emptyness of the set defined by [setp] (which must be of type
    [ty -> bool]).
*)


val mk_subtype:
  Scope.t -> string -> string list
  -> Gtype.t -> Term.term -> string -> string
  -> subtype_defn
(** [mk_subtype scp n args ty set rep abs]: Make a subtype named [n]
    with parameters [args] from type [ty]. Term [set] is the defining
    set and must be of type [ty -> bool]. Strings [rep] and [abs] are
    the names to use for the representation and abstraction functions.
*)


(**
   Support for definition parsers.
*)
module Parser:
sig
  (** Support for definition parsers.  *)

  (** Information to be returned by type definition parsers.  *)
  type typedef =
    | NewType of (string * (string list))
    (** A new type: the type name and its arguments. *)
    | TypeAlias of (string * (string list) * Gtype.t)
    (** A type alias: the type name, its arguments and the type it
        aliases *)
    | Subtype of ((string * (string list) * Gtype.t * Term.term))
(** Subtype definition: The type name, its arguments, the type it
    subtypes and the defining predicate
*)

end

(** Debugging information. *)
val mk_all_from_list:
  Scope.t -> Term.term -> Term.term list -> Term.term
