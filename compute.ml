open Interface
open Ext_tools
open Heuristics

(* TODO : Remove all List.nth and scans from Ext_tools and Compute (use arrays?) *)

(*
event IDs are :
 >= 0 for factual events (match with indexes in the factual trace)
 < 0 for counterfactual-only events

step : our step type with an index and an ID
tstep : step of the Trace module, with only a partial ID
cstep : counterfactual step of the resimulator, with only a partial ID
trace : list of step
ttrace : list of tstep
ctrace : list of cstep
core : list of indexes
subtrace : same as a trace, but representss a core

prefix f : factual (often omitted)
prefix cf : countefactual

cf_part : see below
*)

type cf_part = (step list) * ((int * int) list) * ((int * Grid.constr * int) list) * ((int * Grid.constr * int) list)
(* (subtrace * precedence arrows * direct causality arrows * inhibition arrows) *)

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  follow_causal_core : bool;
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows : int;
  more_relations_with_factual : bool;
  show_entire_counterfactual_stories : bool;
}

let rec get_eoi model rule_name trace = match trace with
  | [] -> raise Not_found
  | s::trace when get_name model s "" = rule_name -> s
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
  | (Resimulation.Factual_happened s)::_ when get_id_of_ts s = eoi_id -> true
  | (Resimulation.Factual_did_not_happen (_,s))::_ when get_id_of_ts s = eoi_id -> false
  | _::ctrace -> trace_succeed eoi_id ctrace
  end
  with Not_found -> trace_succeed eoi_id (List.tl ctrace)

let resimulate_and_sample model nb eoi_id block_pred stop_pred ttrace =
  let rec aux nb (nb_failed,wit) = match nb with
    | 0 -> (nb_failed,wit)
    | n ->
    let ctrace = resimulate model block_pred stop_pred ttrace in
    if trace_succeed eoi_id ctrace then
    aux (n-1) (nb_failed,wit)
    else
    aux (n-1) (nb_failed+1,Some ctrace)
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

let rec last_inhibitive_event_before index (grid,vi) constr =
  try
  (
    let Grid.Constr (var, _) = constr in
    let history = (Hashtbl.find vi (Grid.Var var)).Causal_core.modified_in_t in
    let last = History.last_before index history in
    match last with
    | None -> None
    | Some (i,_) -> let (_,actions) = grid.(i) in
    if List.exists (fun c -> c=constr) actions
    then None (* It is an activation *)
    else (
      let prev = last_inhibitive_event_before i (grid,vi) constr in
      if prev = None then Some i else prev
    )
  ) with Not_found -> None

let rec first_activation_event_between index1 index2 (grid,vi) constr among =
  try
  (
    let Grid.Constr (var, _) = constr in
    let history = (Hashtbl.find vi (Grid.Var var)).Causal_core.modified_in_t in
    let last = History.first_after index1 history in
    match last with
    | None -> None
    | Some (i,_) when i >= index2 -> None
    | Some (i,_) -> let (_,actions) = grid.(i) in
    if List.exists (fun c -> c=constr) actions
    then
    ( match among with
      | None -> Some i
      | Some lst -> if List.exists (fun i' -> i=i') lst then Some i else first_activation_event_between i index2 (grid,vi) constr among
    ) 
    else first_activation_event_between i index2 (grid,vi) constr among
  ) with Not_found -> None

let activation_event_between index1 index2 (grid,vi) constr prefer_core =
  let i = first_activation_event_between index1 index2 (grid,vi) constr None in
  if prefer_core <> None && i <> None
  then
  (
    let i' = first_activation_event_between index1 index2 (grid,vi) constr prefer_core in
    if i' = None then i else i'
  )
  else i

type inhibition_reason =
  | Inhibition of int * Grid.constr * int
  | No_reason of int

let find_inhibitive_arrows trace1 trace2 (grid1,vi1) (grid2,vi2) eoi1 follow_core =
  let rec rewind (src,c,dest) =
    let src_event = List.nth trace2 src in
    let index1_eq = (nb_of_events_before_time_large trace1 (get_time src_event 0.0))-1 in
    let act = activation_event_between index1_eq dest (grid1,vi1) c follow_core in
    match act with
    | None -> [Inhibition (src,c,dest)]
    | Some i -> aux src_event
  and aux dest_event =
    let (tests, _) = grid1.(get_index dest_event) in
    let index2_eq = nb_of_events_before_time_strict trace2 (get_time dest_event 0.0) in
    let inh = List.map (fun c -> (last_inhibitive_event_before index2_eq (grid2,vi2) c, c)) tests in
    let inh = List.filter (fun (opt,c) -> opt <> None) inh in
    let inh = List.map (fun (Some src,c) -> (src,c,get_index dest_event)) inh in
    match inh with
    | [] -> [No_reason (get_index dest_event)]
    | inh -> List.flatten (List.map rewind inh)
  in aux eoi1

let factual_events_of_arrows arrows =
  let involved = List.flatten (List.map (fun (e1,c,e2) -> [e1;e2]) arrows) in
  IntSet.of_list (List.filter (fun x -> x >= 0) involved)

let factual_events_of_trace trace =
  let steps = List.filter (fun s -> get_id s >= 0) trace in
  IntSet.of_list (List.map get_id steps)

 let add_cf_parts model (trace,ttrace) (grid,vi) eoi config core =
   let rec aux core cf_parts events_in_factual =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    log "Choosing interventions..." ;
    let interventions = heuristic_block_all trace core in
    let scs = [Event_has_happened (get_id eoi);Event_has_not_happened (get_id eoi)] in
    (* Compute and sample counterfactual traces (resimulation stops when eid has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness
    (heuristic? random among the traces that block the eoi? smallest core? more blocked event?) *)
    log "Resimulating..." ;
    let (nb_failed,ctrace) = resimulate_and_sample model config.nb_samples (get_id eoi) interventions scs ttrace in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    log ("Ratio : "^(string_of_float ratio)) ;
    if ratio >= config.threshold then (core, cf_parts)
    else
    (
      log "Computing counterfactual experiment..." ;
      (* Set IDs for counterfactual events & conversions *)
      let Some ctrace = ctrace in
      let ctrace = set_ids_of_ctrace ctrace in
      let cf_trace = ctrace_to_trace ctrace in
      let cf_ttrace = trace_to_ttrace cf_trace in
      (* Find the inhibitors of eoi in the cf trace *)
      (* If there is no inhibitors, we add direct cause blocked to the core in order to avoid infinite loop *)
      let (inhibited_tests, _) = grid.(get_id_of_ts inhibited_ts) in
      let inhibited_time = get_time_of_ts inhibited_ts 0.0 in
      let inhibited_cf_index = nb_of_events_before_time cf_trace inhibited_time in
      let (cf_grid,cf_vi) = compute_trace_infos model cf_ttrace (inhibited_cf_index-1) in
      let inhibitive_indexes = find_inhibitive_events (cf_grid,cf_vi) inhibited_tests inhibited_cf_index in
      (* Eventually keep only the first (earliest) of these events, and compute their causal core.
      Add this counterfactual causal core to the list and indicate where go the inhibitions arrows. *)
      let inhibitive_indexes = if config.allow_multiple_cf_inhibition_arrows then inhibitive_indexes
      else [list_min_c (fun (i,const) (i',constr') -> compare i i') inhibitive_indexes] in
      let inhibitions_cf = List.map (fun (cf_eoi_index, cf_eoi_constr) ->
      index_to_id cf_trace cf_eoi_index, cf_eoi_constr, get_id_of_ts inhibited_ts) inhibitive_indexes in
      let cf_core = compute_causal_core model (cf_grid,cf_vi)
      (List.map (fun (cf_eoi_index, cf_eoi_constr) -> cf_eoi_index) inhibitive_indexes) in
      let cf_subtrace = core_to_subtrace cf_trace cf_core in
      (* For each direct causal relation between a counterfactual-only event and a factual event of the counterfactual core,
      find the last events in the factual trace that prevent it (same method as above, depending on the config).
      Indicate in the counterfactual core the origin of these inhibition arrows. *)
      (* TODO : With the new algorithm, we should search inhibitive events of counterfactual eois. *)
      let activations = Precedence.compute_strong_deps model cf_grid cf_core in
      let inhibitions_fc = List.map (find_fc_inhibition_arrow trace (grid,vi) cf_trace) activations in
      let inhibitions_fc = List.flatten inhibitions_fc in
      let inhibitions_fc = begin match config.fc_inhibition_arrows with
      | All -> inhibitions_fc
      | One -> [list_min_c (fun (a,c,b) (a',c',b') -> compare a a') inhibitions_fc]
      | Max_one_per_event -> List.map (fun x -> list_min_c (fun (a,c,b) (a',c',b') -> compare a a') x) (group_arrows_by_dest inhibitions_fc)
      end in
      let activations = List.map (fun (i1,c,i2) -> (index_to_id cf_trace i1,c,index_to_id cf_trace i2)) activations in
      let precedences = Precedence.transitive_reduction (Precedence.compute_precedence cf_ttrace cf_grid cf_core) in
      let precedences = List.map (fun (i1,i2) -> (index_to_id cf_trace i1,index_to_id cf_trace i2)) precedences in
      (* TODO : Update the counterfactual core also. *)
      (* Update the factual core : compute a new factual causal core with all the previous added events + factual events with an inhibitive arrow
      + other factual events of the counterfactual core if we want to have more links with the factual core at the end. *)
      log "Updating factual core..." ;
      let inhibitions = inhibitions_cf@inhibitions_fc in
      let events_in_factual = IntSet.union (factual_events_of_arrows inhibitions) events_in_factual in
      let events_in_factual = if config.more_relations_with_factual
      then IntSet.union (factual_events_of_trace cf_subtrace) events_in_factual
      else events_in_factual in
      let core = compute_causal_core model (grid,vi) (IntSet.elements events_in_factual) in
      let cf_parts = (cf_subtrace,precedences,activations,inhibitions)::cf_parts in
      aux core cf_parts events_in_factual
    )
   in aux core [] (IntSet.singleton (get_index eoi))

let compute_extended_story model ttrace rule_name config =
  log "Computing initial factual core..." ;
  (* We have to set IDs in the trace and convert it *)
  let ttrace = List.mapi set_id_of_ts ttrace in
  let trace = ttrace_to_trace ttrace in
  (* Determining event of interest and truncate the traces *)
  let eoi = get_eoi model rule_name trace in
  let trace = cut_after_index (get_index eoi) trace in
  let ttrace = cut_after_index (get_index eoi) ttrace in
  (* Computing factual causal core *)
  let infos = compute_trace_infos model ttrace (get_index eoi) in
  let core = compute_causal_core model infos [(get_index eoi)] in
  (* Adding counterfactual parts *)
  let (core, cf_parts) = add_cf_parts model (trace,ttrace) infos eoi config core in
  (* Merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
      - Merge everything by merging together nodes that represent the same event
      - Keep only counterfactual-only events of counterfactual cores
      Don't forget to put all the inhibition arrows that had been found and to compute relations for the factual core and each counterfactual parts.
  *)
  log "Finished !" ;
  (* TODO *)
  ()
