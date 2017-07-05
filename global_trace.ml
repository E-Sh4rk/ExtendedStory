open Ext_tools

(* Abstract types *)
type global_info = (int*int) array (* Associate an index with (id,order) *)
type trace_info = Trace_explorer.t * Causal_core.var_info_table
type global_trace = trace_info * global_info

type global_info_builder = ( (int*int) list ) * int (* (list,next_rt_index,next_order) *)
type global_trace_builder = (Trace.step list) * global_info_builder

(* Not exported *)
let set_id (_,gi) index id =
  let (_,order) = gi.(index) in
  gi.(index) <- (id,order)

let set_order (_,gi) index order =
  let (id,_) = gi.(index) in
  gi.(index) <- (id,order)

(* Exported *)

(* Basic functions *)
let get_id (_,gi) index =
  let (id,_) = gi.(index) in
  id

let get_order (_,gi) index =
  let (_,order) = gi.(index) in
  order

let get_step ((te,_),_) index = Trace_explorer.step index te

let length (_,gi) = Array.length gi

let get_trace_explorer ((te,_),_) =  te

let new_reference_trace te =
  Trace_explorer.Grid.build te ;
  let vi = Causal_core.init_var_infos te in
  let gi = Array.make ((Trace_explorer.last_step_id te) + 1) (0,0) in
  let gtr = ((te,vi),gi) in
  for i=0 to (Array.length gi) - 1 do
    set_id gtr i i ;
    set_order gtr i i
  done ; gtr

let new_counterfactual_trace_builder () = ([],([],0,0))

let last_cf_id = ref 0
let add_counterfactual_step rtr (csteps,(cgi,ri,o)) cs =
  match cs with
  | Resimulation.Counterfactual_happened s ->
  last_cf_id := !last_cf_id - 1 ;
  (s::csteps,((-(!last_cf_id),o)::cgi,ri,o+1))
  | Resimulation.Factual_did_not_happen (_,_) ->
  set_order rtr ri o ; (csteps,(cgi,ri+1,o+1))
  | Resimulation.Factual_happened s ->
  set_order rtr ri o ;
  (s::csteps,((get_id rtr ri,o)::cgi,ri+1,o+1))

let finalize_counterfactual_trace rtr (ctr,(cgi,ri,o)) =
  for i=ri to (length rtr) - 1 do
    set_order rtr i (o+i-ri)
  done ;
  let te = Trace_explorer.of_trace (Trace_explorer.model (get_trace_explorer rtr)) (List.rev ctr) in
  Trace_explorer.Grid.build te ;
  let vi = Causal_core.init_var_infos te in
  ((te,vi), Array.of_list (List.rev cgi))

(* Search functions : search ID, search order, search time... *)

(* Trace functions : get_tests / get_actions, get_var_infos *)
