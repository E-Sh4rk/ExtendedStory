(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
(*                                                                            *)
(*  Author / Maintainer: Jonathan Laurent (jonathan.laurent@cs.cmu.edu)       *)
(*                                                                            *)
(******************************************************************************)

(** Resimulation algorithm that serves as a basis for 
    counterfactual reasoning. *)

type state

type step =
  | Factual_happened of Trace.step
  | Factual_did_not_happen of bool * Trace.step
  | Counterfactual_happened of Trace.step

val debug_print_resimulation_step : Model.t -> Format.formatter -> step -> unit

exception End_of_resimulation

val init : Model.t -> Random.State.t -> state

val do_step : 
  (Trace.step * float * bool) option -> state -> 
  bool * step option * state
(** [do_step next_reference_step state]
    @return [(reference_step_consummed, counterfactual_step, new_state)]

    Performs one step of resimulation. The first argument should be
    [Some (step, time, blocked)] where [step] is the next step in the 
    reference trace, [time] is the time at which it happens, and [block]
    is true iff it should be blocked. It should be [None] if there are no
    more events in the reference trace.
    This function returns a boolean which is
    true iff the given reference step has been consummed, an optional
    resimulation step ([None] in case of a null event) and a new state.

    The exception [End_of_resimulation] is raised if there are no
    more events in the reference trace and the divergent activity is 0. *)


val resimulate : 
  ?stop_after:(step -> bool) ->
  blocked:(Model.t -> Trace.step -> bool) -> 
  rcv_step:(Model.t -> step -> unit) ->
  string -> unit
(** [resimulate ?stop_after ~blocked ~rcv_step trace_file]

    Resimulates the trace contained in [trace_file], blocking
    steps for which predicate [blocked] is true. Resimulation
    steps are communicated throug the [rcv_step] handler. *)
