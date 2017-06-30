open Interface
open Ext_tools

(*
event IDs are :
 >= 0 for factual events (match with indexes)
 < 0 for counterfactual-only events

step : our step type with an index and an ID
tstep : step of the Trace module, with only a partial ID
cstep : counterfactual step of the resimulator, with only a partial ID
trace : list of step
ttrace : list of tstep
ctrace : list of cstep
core : list of indexes
subtrace : same as a trace, but representss a core

prefix f : factual
prefix cf : countefactual

cf_part : see below
*)

type cf_part = (step list) * ((int * int) list) * ((int * Grid.constr * int) list) * ((int * Grid.constr * int) list)
(* (events * precedence arrows * direct causality arrows * inhibition arrows) *)

type inhibition_arrows_limitation = One | Max_one_per_event | All

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  inhibition_arrows : inhibition_arrows_limitation;
  more_relations_with_factual : bool;
  show_entire_counterfactual_stories : bool;
}

let heuristic_choose_interventions () : interventions = [] (* TODO *)

exception Not_found

let rec get_eoi model rule_name trace = match trace with
  | [] -> raise Not_found
  | s::trace when get_name model s "" = rule_name -> get_id s
  | s::trace -> get_eoi model rule_name trace

let compute_trace_infos model ttrace last_eoi =
  let (grid, _) = Grid.build_grid model ttrace in
  let var_infos = Causal_core.var_infos_of_grid model grid last_eoi in
  (grid,var_infos)

let compute_causal_core model (grid,var_infos) eois_indexes =
  Causal_core.core_events (Causal_core.causal_core_of_eois model grid var_infos eois_indexes)

let rec trace_succeed eoi_id ctrace =
  try begin match ctrace with
  | [] -> false
  | (Resimulation.Factual_happened s)::ctrace when get_id_of_ts s = eoi_id -> true
  | (Resimulation.Factual_did_not_happen (_,s))::ctrace when get_id_of_ts s = eoi_id -> false
  | _::ctrace -> trace_succeed eoi_id ctrace
  end
  with Not_found -> trace_succeed eoi_id (List.tl ctrace)

let resimulate_and_sample model nb eoi_id block_pred stop_pred ttrace =
  let rec aux nb (nb_failed,wit) = match nb with
    | 0 -> (nb_failed,wit)
    | n ->
    let ctrace = resimulate model block_pred stop_pred ttrace in
    if trace_succeed eoi_id ctrace then
    aux (nb-1) (nb_failed,wit)
    else
    aux (nb-1) (nb_failed+1,Some ctrace)
  in aux nb (0,None)

let last_counterfactual_id = ref 0
let set_ids_of_ctrace ctrace =
  let rec aux ctrace = match ctrace with
  | [] -> []
  | (Resimulation.Counterfactual_happened ts)::ctrace ->
  last_counterfactual_id := (!last_counterfactual_id) - 1 ;
  (Resimulation.Counterfactual_happened (set_id_of_ts (!last_counterfactual_id) ts))::(aux ctrace)
  | cs::ctrace -> cs::(aux ctrace)
  in aux ctrace
 
let rec first_inhibited_event f_core ctrace = match ctrace with
  | [] -> failwith "First inhibited event not found !"
  | (Resimulation.Factual_did_not_happen (blocked, ts))::ctrace when not blocked
  -> begin
  try (
    let id = get_id_of_ts ts in
    if id >= 0 && List.exists (fun i -> i=id) f_core
    then ts else first_inhibited_event f_core ctrace
  ) with Not_found -> first_inhibited_event f_core ctrace end
  | _::ctrace -> first_inhibited_event f_core ctrace

let rec last_inhibitive_event_before index (grid,vi) constr =
  try
  (
    let Grid.Constr (var, value) = constr in
    let history = (Hashtbl.find vi (Grid.Var var)).Causal_core.modified_in_t in
    let last = History.last_before index history in
    match last with
    | None -> None
    | Some (i,_) -> let (test,actions) = grid.(i) in
    if List.exists (fun c -> c=constr) actions
    then None (* It is an activation *)
    else (
      let prev = last_inhibitive_event_before i (grid,vi) constr in
      if prev = None then Some (i,constr) else prev
    )
  ) with Not_found -> None

let find_inhibitive_events (grid,vi) tests before_index =
  let events = List.map (last_inhibitive_event_before before_index (grid,vi)) tests in
  let events = List.filter (fun opt -> opt <> None) events in
  List.map (fun (Some i) -> i) events

let core_to_subtrace trace core =
  let rec aux i core trace = match core, trace with
  | [], _ -> []
  | index::core, s::trace when index=i -> s::(aux (i+1) core trace)
  | core, s::trace -> aux (i+1) core trace
  | _, _ -> failwith "Invalid core !"
  in aux 0 core trace

let find_fc_inhibition_arrow trace (grid,vi) ctrace (index1, constr, index2) =
  let ev1 = List.nth ctrace index1
  and ev2 = List.nth ctrace index2 in
  let id1 = get_id ev1
  and id2 = get_id ev2 in
  if id1 < 0 || id2 >= 0 then []
  else (
    let time = get_time ev2 0.0 in
    let findex = nb_of_events_before_time trace time in
    List.map (fun (i,constr) -> (i,constr,id2)) (find_inhibitive_events trace (grid,vi) [constr] findex)
  )

let group_arrows_by_dest lst =
  let rec aux lst = match lst with
  | [] -> []
  | (s,c,d)::(s',c',d')::lst when d=d' -> let fg::gs = (aux ((s',c',d')::lst)) in
  ((s,c,d)::fg)::gs
  | a::lst -> [a]::(aux lst) in
  aux (List.sort (fun (src,c,dest) (src',c',dest') -> compare dest dest') lst)

let factual_events_of_arrows arrows =
  let involved = List.flatten (List.map (fun (e1,c,e2) -> [e1;e2]) arrows) in
  List.filter (fun x -> x >= 0) involved

let factual_events_of_trace steps =
  let steps = List.filter (fun s -> try get_id s >= 0 with Not_found -> false) steps in
  List.map get_id steps

 let add_cf_parts model (trace,ttrace) (grid,vi) eoi_id config f_core =
   let rec aux f_core cf_parts events_in_factual =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core :
    For example :
    - Block permanently in trace T every event that involve species in the factual core and that is not in the factual causal core.
    - Block permanently an event that is suspected to have an impact later.*)
    let interventions = heuristic_choose_interventions () in
    let block_pred = interventions_to_predicate interventions
    and stop_pred = stop_conditions_to_predicate [Event_has_happened eoi_id;Event_has_not_happened eoi_id] in
    (* Compute and sample counterfactual traces (resimulation stops when eid has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness
    (heuristic? random among the traces that block the eoi? smallest core? more blocked event?) *)
    let (nb_failed,ctrace) = resimulate_and_sample model config.nb_samples eoi_id block_pred stop_pred ttrace in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    if ratio >= config.threshold then (f_core, cf_parts)
    else
    (
      (* Don't forget to set IDs for counterfactual events *)
      let Some ctrace = ctrace in
      let ctrace = set_ids_of_ctrace ctrace in
      (* Convert the counterfactual trace to a regular trace *)
      let cf_trace = ctrace_to_trace ctrace in
      let cf_ttrace = trace_to_ttrace cf_trace in
      (* Find the last events that have inhibited the first event of the causal core that has been inhibited :
      it is the last events that changed the value of a tested logical site from a good value to a wrong value. *)
      let inhibited_ts = first_inhibited_event f_core ctrace in
      let (inhibited_tests, _) = grid.(get_id_of_ts inhibited_ts) in
      let inhibited_time = get_time_of_ts inhibited_ts 0.0 in
      let inhibited_cf_index = nb_of_events_before_time cf_trace inhibited_time in
      let (cf_grid,cf_vi) = compute_trace_infos model cf_ttrace (inhibited_cf_index-1) in
      let inhibitive_indexes = find_inhibitive_events (cf_grid,cf_vi) inhibited_tests inhibited_cf_index in
      (* Select the first (earliest) of these events and compute its causal core. Add this counterfactual causal core to the list and indicate where go the inhibition arrow. *)
      (***************************************TODO*****************************************)
      let (ceoi_index,ceoi_constr) = list_min_c (fun (i,const) (i',constr') -> compare i i') inhibitive_events in
      let ccore = compute_causal_core model (cgrid,cvi) [ceoi_index] in
      let inhibition_arrow = (get_id (List.nth reg_ctrace ceoi_index), ceoi_constr, get_id inhibited_event) in
      let csubtrace = core_to_subtrace reg_ctrace ccore in 
      (* For each direct causal relation between a counterfactual-only event and a factual event of the counterfactual core,
      find the last events in the factual trace that prevent it (same method as above, depending on the config).
      Indicate in the counterfactual core the origin of these inhibition arrows. *)
      let activations = Precedence.compute_strong_deps model cgrid ccore in
      let inhibitions = List.map (find_fc_inhibition_arrow trace (grid,vi) reg_ctrace) activations in
      let inhibitions = List.flatten inhibitions in
      let inhibitions = begin match config.inhibition_arrows with
      | All -> inhibitions
      | One -> [list_min_c (fun (a,c,b) (a',c',b') -> compare a a') inhibitions]
      | Max_one_per_event -> List.map (fun x -> list_min_c (fun (a,c,b) (a',c',b') -> compare a a') x) (group_arrows_by_dest inhibitions)
      end in
      let activations = List.map index_arrow_to_id_arrow activations in
      let precedences = Precedence.transitive_reduction (Precedence.compute_precedence reg_ctrace cgrid ccore) in
      let precedences = List.map index_arrow_to_id_arrow activations in
      (* Update the factual core : compute a new factual causal core with all the previous added events + factual events with an inhibitive arrow
      + other factual events of the counterfactual core if we want to have more links with the factual core at the end. *)
      let inhibitions_arrows = inhibition_arrow::inhibitions in
      let events_in_factual = (factual_events_of_arrows inhibitions_arrows) @ events_in_factual in
      let events_in_factual = if config.more_relations_with_factual
      then (factual_events_of_trace csubtrace) @ events_in_factual
      else events_in_factual in
      let events_in_factual = List.sort_uniq compare events_in_factual in
      let f_core = compute_causal_core model (grid,vi) events_in_factual in
      let cf_parts = (csubtrace,precedences,activations,inhibitions_arrows)::counterfactuals in
      aux f_core cf_parts events_in_factual
    )
   in aux f_core [] [eoi_id]

  (* TODO : Adapt above code to new ID system *)

let compute_extended_story model ttrace rule_name config =
  (* We have to set IDs in the trace and convert it *)
  let ttrace = List.mapi set_id_of_ts i s ttrace in
  let trace = ttrace_to_trace ttrace in
  (* Determining event of interest *)
  let eoi_id = get_eoi model rule_name trace in
  (* Computing factual causal core *)
  let infos = compute_trace_infos model ttrace eoi_id in
  let core = compute_causal_core model infos [eoi_id] in
  (* Adding counterfactual parts *)
  let (core, cf_parts) = add_cf_parts model (trace,ttrace) infos eoi_id config core in
  (* Merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
      - Merge everything by merging together nodes that represent the same event
      - Keep only counterfactual-only events of counterfactual cores
      Don't forget to put all the inhibition arrows that had been found and to compute relations for the factual core and each counterfactual parts.
  *)
  (* TODO *)
  ()
