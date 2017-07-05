open Ext_tools

(* Abstract types *)
type global_info = (int*int) array (* Associate an index with (id,order) *)
type global_trace = (Trace.step array) * global_info

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
let get_id (_,gi) index =
  let (id,_) = gi.(index) in
  id

let get_order (_,gi) index =
  let (_,order) = gi.(index) in
  order

let get_step (tr,_) index = tr.(index)

let get_local_trace (tr,_) = Array.to_list tr

let new_reference_trace tr =
  let tr = Array.of_list tr in
  let gi = Array.make (Array.length tr) (0,0) in
  for i=0 to (Array.length gi) - 1 do
    set_id (tr,gi) i i ;
    set_order (tr,gi) i i
  done ; (tr,gi)

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

let finalize_counterfactual_trace (rtr,rgi) (ctr,(cgi,ri,o)) =
  for i=ri to (Array.length rgi) - 1 do
    set_order (rtr,rgi) i (o+i-ri)
  done ;
  (Array.of_list (List.rev ctr), Array.of_list (List.rev cgi))

(* Search functions : search ID, search order, search time... *)

(* Trace functions : get_tests / get_actions (grid), get_var_infos *)
