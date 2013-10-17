(**----
   Name: userlib.mli
   Copyright M Wahab 2013
   Author: M Wahab  <mwb.cde@gmail.com>

   This file is part of HSeq

   HSeq is free software; you can redistribute it and/or modify it under
   the terms of the Lesser GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   HSeq is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the Lesser GNU General Public
   License for more details.

   You should have received a copy of the Lesser GNU General Public
   License along with HSeq.  If not see <http://www.gnu.org/licenses/>.
   ----*)

(** {5 Default values} *)
module Default :
sig
  val context: unit -> Context.t
  val scope: unit -> Scope.t
  val printers: unit -> Printer.ppinfo
  val parsers: unit -> Parser.Table.t
  val simpset: unit -> Simpset.simpset
  val proofstack: unit -> Goals.ProofStack.t
  val base_thy_builder: unit -> unit -> unit
end

(** {5 Global state} *)

module State: 
sig
  type t =
    {
      context_f : Context.t;
      scope_f : Scope.t;
      simpset_f : Simpset.simpset;
      proofstack_f: Goals.ProofStack.t;
      base_thy_builder_f: t -> t
    }
    
  val empty: unit -> t

  val context: t -> Context.t
  val set_context: t -> Context.t -> t

  val scope: t -> Scope.t
  val set_scope: t -> Scope.t -> t

  val ppinfo: t -> Printer.ppinfo
  val set_ppinfo: t -> Printer.ppinfo -> t

  val parsers: t -> Parser.Table.t
  val set_parsers: t -> Parser.Table.t -> t

  val simpset: t -> Simpset.simpset
  val set_simpset: t -> Simpset.simpset -> t

  val proofstack: t -> Goals.ProofStack.t
  val set_proofstack: t -> Goals.ProofStack.t -> t

  val base_thy_builder: t -> (t -> t)
  val set_base_thy_builder: t -> (t -> t) -> t
end

val state: unit -> State.t
(** The global state *)
val set_state: State.t -> unit
(** Set the global state *)

val context: State.t -> Context.t
(** The global context *)
val set_context: State.t -> Context.t -> State.t
  (** Set the global context *)

val scope: State.t -> Scope.t
  (** The global Scope *)
val set_scope: State.t -> Scope.t -> State.t
  (** Set the global scope *)

val ppinfo: State.t -> Printer.ppinfo
  (** The global pretty printers *)
val set_ppinfo: State.t -> Printer.ppinfo -> State.t
  (** Set the global pretty printers *)

val parsers: State.t -> Parser.Table.t
  (** The global parser tables *)
val set_parsers: State.t -> Parser.Table.t -> State.t
  (** Set the global parser tables *)

val simpset: State.t -> Simpset.simpset
  (** The standard simplifier set *)
val set_simpset: State.t -> Simpset.simpset -> State.t
  (** Set the global simplifier set *)

val proofstack: State.t -> Goals.ProofStack.t
(** The standard simplifier set *)
val set_proofstack: State.t -> Goals.ProofStack.t -> State.t
(** Set the global simplifier set *)

val base_thy_builder: State.t -> (State.t -> State.t)
(** The base theory builder *)
val set_base_thy_builder: State.t -> (State.t -> State.t) -> State.t
(** Set the base theory builder *)


val init_context: State.t -> State.t
  (** Initialize the global context *)
val init_scope: State.t -> State.t
  (** Initialize the global scope *)
val init_ppinfo: State.t -> State.t
  (** Initialize the global pretty printers *)
val init_parsers: State.t -> State.t
  (** Initialize the global parser tables *)
val init_simpset: State.t -> State.t
(** Initialize the global simpset *)
val init_proofstack: State.t -> State.t
(** Initialize the global proofstack *)
val init_base_thy_builder: State.t -> State.t
(** Initialize the base theory builder *)

module Access :
sig
  val context: unit -> Context.t
  (** The global context *)
  val set_context: Context.t -> unit
  (** Set the global context *)
  val init_context: unit -> unit
  (** Initialize the global context *)

  val scope: unit -> Scope.t
  (** The global Scope *)
  val set_scope: Scope.t -> unit
  (** Set the global scope *)
  val init_scope: unit -> unit
  (** Initialize the global scope *)

  val ppinfo: unit -> Printer.ppinfo
  (** The global pretty printers *)
  val set_ppinfo: Printer.ppinfo -> unit
  (** Set the global pretty printers *)
  val init_ppinfo: unit -> unit
  (** Initialize the global pretty printers *)

  val parsers: unit -> Parser.Table.t
  (** The global parser tables *)
  val set_parsers: Parser.Table.t -> unit
  (** Set the global parser tables *)
  val init_parsers: unit -> unit
  (** Initialize the global parser tables *)

  val simpset: unit -> Simpset.simpset
  (** The standard simplifier set *)
  val set_simpset: Simpset.simpset -> unit
  (** Set the global simplifier set *)
  val init_simpset: unit -> unit
(** Initialize the global simpset *)

  val proofstack: unit -> Goals.ProofStack.t
  (** The standard proofstack *)
  val set_proofstack: Goals.ProofStack.t -> unit
  (** Set the global proofstack *)
  val init_proofstack: unit -> unit
(** Initialize the global proofstack *)
end

