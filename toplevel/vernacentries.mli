
(* $Id$ *)

(*i*)
open Names
open Term
open Vernacinterp
(*i*)

(* Vernacular entries. This module registers almost all the vernacular entries,
   by side-effects using [Vernacinterp.vinterp_add]. *)

val join_binders : ('a list * 'b) list -> ('a * 'b) list
val add : string -> (vernac_arg list -> unit -> unit) -> unit
val show_script : unit -> unit
val show_prooftree : unit -> unit
val show_open_subgoals : unit -> unit
val show_nth_open_subgoal : int -> unit
val show_open_subgoals_focused : unit -> unit
val show_node : unit -> unit

