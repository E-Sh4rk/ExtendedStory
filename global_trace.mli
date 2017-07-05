
type global_trace
type global_trace_builder

val get_global_id : global_trace -> int -> int
val get_order : global_trace -> int -> int
val get_step : global_trace -> int -> Trace.step
val length : global_trace -> int
val get_trace_explorer : global_trace -> Trace_explorer.t
val new_reference_trace : Trace_explorer.t -> global_trace
val new_counterfactual_trace_builder : unit -> global_trace_builder
val add_counterfactual_step : global_trace -> global_trace_builder -> Resimulation.step -> global_trace_builder
val finalize_counterfactual_trace : global_trace -> global_trace_builder -> global_trace

val search_global_id : global_trace -> int -> int option (* Linear operation, logarithmic in general *)
val search_first_after_order : global_trace -> int -> int option (* Logarithmic operation *)
val search_last_before_order : global_trace -> int -> int option (* Logarithmic operation *)

val get_tests : global_trace -> int -> Grid.constr list
val get_actions : global_trace -> int -> Grid.constr list
val get_history : global_trace  -> 'a Grid.var -> History.t
