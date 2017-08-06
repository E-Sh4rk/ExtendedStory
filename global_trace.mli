
type t
type t_builder

(**
Get the global ID of the event at the given index (constant time).
Note that event IDs are :
 >= 0 for factual events (match with indexes in the initial factual trace)
 < 0 for counterfactual-only events
*)
val get_global_id : t -> int -> int
(** Get the global order of happening of the event at the given index. *)
val get_order : t -> int -> int
(** Some accessors for the event at the given index. *)
val get_step : t -> int -> Trace.step
val length : t -> int
val get_trace_explorer : t -> Trace_explorer.t
val get_model : t -> Model.t

(**
Initialize a new global trace of reference from a trace explorer.
If you want that the global IDs coincide with the indexes, call reset_ids before
(but be carefull : it will invalidate previous global traces).
*)
val new_reference_trace : Trace_explorer.t -> t
(**
Copy a global trace.
Can be used to save a synchronized reference global trace before
synchronizing it with a new counterfactual global trace.
*)
val copy : t -> t
(**
Create a global trace that is a subtrace of the given global trace.
IDs and orders are preserved, and the subtrace returned is a copy.
*)
val subtrace_of : t -> int list -> t
(**
Following methods are used to create and synchronize a new counterfactual global trace
with the reference global trace.
/!\ The reference global trace will be unsychronized with the previous counterfactual trace.
*)
val new_counterfactual_trace_builder : unit -> t_builder
val add_counterfactual_step : t -> t_builder -> Resimulation.step -> t_builder
val finalize_counterfactual_trace : t -> t_builder -> t

(**
Search functions.
/!\ Not constant in time !
*)
val search_global_id : t -> int -> int option (* Linear operation, logarithmic in general *)
val search_first_after_order : t -> int -> int option (* Logarithmic operation *)
val search_last_before_order : t -> int -> int option (* Logarithmic operation *)

(** Some other accessors... *)
val get_tests : t -> int -> Grid.constr list
val get_actions : t -> int -> Grid.constr list
val get_var_infos : t -> Causal_core.var_info_table
val get_history : t  -> 'a Grid.var -> History.t

(** Misc *)
val reset_ids : unit -> unit

(** Printing and debug functions. *)
val print_core : t -> Format.formatter -> int list -> unit
val print : Format.formatter -> t -> unit
val print_full : Format.formatter -> t -> unit
