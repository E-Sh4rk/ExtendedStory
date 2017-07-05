
type t
type t_builder

val get_global_id : t -> int -> int
val get_order : t -> int -> int
val get_step : t -> int -> Trace.step
val length : t -> int
val get_trace_explorer : t -> Trace_explorer.t
val get_model : t -> Model.t
val new_reference_trace : Trace_explorer.t -> t
val new_reference_subtrace : t -> int list -> t
val new_counterfactual_trace_builder : unit -> t_builder
val add_counterfactual_step : t -> t_builder -> Resimulation.step -> t_builder
val finalize_counterfactual_trace : t -> t_builder -> t

val search_global_id : t -> int -> int option (* Linear operation, logarithmic in general *)
val search_first_after_order : t -> int -> int option (* Logarithmic operation *)
val search_last_before_order : t -> int -> int option (* Logarithmic operation *)

val get_tests : t -> int -> Grid.constr list
val get_actions : t -> int -> Grid.constr list
val get_history : t  -> 'a Grid.var -> History.t
