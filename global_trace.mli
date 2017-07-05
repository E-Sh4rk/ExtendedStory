
type global_trace
type global_trace_builder

val get_id : global_trace -> int -> int
val get_order : global_trace -> int -> int
val get_step : global_trace -> int -> Trace.step
val length : global_trace -> int
val get_trace_explorer : global_trace -> Trace_explorer.t
val new_reference_trace : Trace_explorer.t -> global_trace
val new_counterfactual_trace_builder : unit -> global_trace_builder
val add_counterfactual_step : global_trace -> global_trace_builder -> Resimulation.step -> global_trace_builder
val finalize_counterfactual_trace : global_trace -> global_trace_builder -> global_trace
