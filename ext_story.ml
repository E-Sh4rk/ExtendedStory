open Interface
open Ext_tools
open Heuristics
open Global_trace

type cf_part = Global_trace.t * ((int * Grid.constr * int) list) (* (subtrace, inhibition arrows) *)
type extended_story = Global_trace.t * (cf_part list) (* (subtrace, counterfactual parts) *)

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  max_counterfactual_parts : int;
  precompute_cf_cores : bool;
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows_per_inhibator : int;
  add_all_factual_events_involved_to_factual_core : bool;
}

let get_eoi rule_name trace =
  let model = get_model trace in
  let rec aux i = match i with
  | i when i >= length trace -> raise Not_found
  | i when get_step_name model (get_step trace i) "" = rule_name -> i
  | i -> aux (i+1) in
  aux 0

let compute_causal_core trace eois =
  let eois = List.sort_uniq Pervasives.compare eois in
  Causal_core.core_events (Causal_core.compute_causal_core (get_trace_explorer trace) (get_var_infos trace) eois)

let cf_trace_succeed eoi_id cf_trace =
  match search_global_id cf_trace eoi_id with
  | None -> false
  | Some _ -> true

let resimulate_and_sample nb eoi block_pred stop_pred trace =
  let rec aux nb (nb_failed,wit) = match nb with
    | 0 -> (nb_failed,wit)
    | n ->
    let cf_trace = resimulate block_pred stop_pred trace in
    if cf_trace_succeed (get_global_id trace eoi) cf_trace then
    aux (n-1) (nb_failed,wit)
    else
    aux (n-1) (nb_failed+1,Some cf_trace)
  in aux nb (0,None)

let rec last_inhibitive_event_before trace index constr =
  let Grid.Constr (var, _) = constr in
  let history = get_history trace var in
  let last = History.last_before index history in
  match last with
  | None -> None
  | Some i ->
  if List.exists (fun c -> c=constr) (get_actions trace i)
  then None (* It is an activation *)
  else (
    if List.exists (fun c -> c=constr) (get_tests trace i)
    then Some i
    else last_inhibitive_event_before trace i constr
  )

let rec last_between_among history among index1 index2 =
  match History.last_before index2 history, among with
  | None, _ -> None
  | Some n, _ when n <= index1 -> None
  | Some n, None -> Some n
  | Some n, Some among when IntSet.mem n among -> Some n
  | Some n, _ -> last_between_among history among index1 n

let rec last_activation_event_between trace among index1 index2 constr =
  let Grid.Constr (var, _) = constr in
  let history = get_history trace var in
  let last = last_between_among history among index1 index2 in
  match last with
  | None -> None
  | Some i ->
  if List.exists (fun c -> c=constr) (get_actions trace i)
  then
  (
    if List.exists (fun c -> c=constr) (get_tests trace i)
    then last_activation_event_between trace among index1 i constr
    else Some i 
  ) 
  else None (* It is an inhibition *)

let activation_event_between trace follow_core index1 index2 constr =
  let i = last_activation_event_between trace None index1 index2 constr in
  if follow_core <> None && i <> None
  then
  (
    let i' = last_activation_event_between trace follow_core index1 index2 constr in
    if i' = None then i else i'
  )
  else i

type inhibition_reason =
  | Inhibition of int * Grid.constr * int
  | No_reason of int

let find_inhibitive_arrows trace1 trace2 follow_core eoi1 =
  let rec rewind (src,c,dest) =
    let index1_eq = search_first_after_order trace1 (get_order trace2 src) in
    let index1_eq = match index1_eq with None -> length trace1 | Some i -> i in
    let act = activation_event_between trace1 follow_core (index1_eq-1) dest c in
    match act with
    | None -> [Inhibition (src,c,dest)]
    | Some i -> aux i
  and aux dest =
    let index2_eq = search_last_before_order trace2 (get_order trace1 dest) in
    let index2_eq = match index2_eq with None -> -1 | Some i -> i in
    let inh = List.map (fun c -> (last_inhibitive_event_before trace2 (index2_eq+1) c, c)) (get_tests trace1 eoi1) in
    let inh = List.filter (fun (opt,c) -> opt <> None) inh in
    let inh = List.map (function (Some src,c) -> (src,c,dest) | _ -> assert false) inh in
    match inh with
    | [] -> [No_reason (dest)]
    | inh -> List.flatten (List.map rewind inh)
  in aux eoi1

let choose_arrows arrows nb =
  let cmp (s,_,d) (s',_,d') = match Pervasives.compare s s' with
  | 0 -> Pervasives.compare d d'
  | n -> n
  in
  let arrows = List.sort cmp arrows in
  cut_after_index (nb-1) arrows

let factual_events_of_trace trace =
  let rec aux acc i = match i with
  | i when i < 0 -> acc
  | i when get_global_id trace i >= 0 -> aux (IntSet.add i acc) (i-1)
  | i -> aux acc (i-1) in
  aux (IntSet.empty) ((length trace)-1)

 let add_cf_parts model (trace,ttrace) (grid,vi) eoi config core =
   let rec aux core cf_parts events_in_factual =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    logs "Choosing interventions..." ;
    let interventions = heuristic_block_all trace core in
    let scs = [Event_has_happened (get_id eoi);Event_has_not_happened (get_id eoi)] in
    (* Compute and sample counterfactual traces (resimulation stops when eid has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness
    (heuristic? random among the traces that block the eoi? smallest core? more blocked event?) *)
    logs "Resimulating..." ;
    let (nb_failed,ctrace) = resimulate_and_sample model config.nb_samples (get_id eoi) interventions scs ttrace in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    logs ("Ratio : "^(string_of_float ratio)) ;
    if ratio >= config.threshold || List.length cf_parts >= config.max_counterfactual_parts then (core, cf_parts)
    else
    (
      logs "Computing counterfactual experiment..." ;
      (* Set IDs for counterfactual events & conversions *)
      let ctrace = match ctrace with Some ctrace -> ctrace | None -> assert false in
      let ctrace = set_ids_of_ctrace ctrace in
      let cf_trace = ctrace_to_trace ctrace in
      let cf_ttrace = trace_to_ttrace cf_trace in
      (* Find the inhibitors of eoi in the cf trace, and eventually filter them *)
      let (cf_grid,cf_vi) = compute_trace_infos model cf_ttrace ((List.length cf_ttrace)-1) in
      let reasons = find_inhibitive_arrows trace cf_trace (grid,vi) (cf_grid,cf_vi) eoi (Some core) in
      let inhibitors_cf_indexes = List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
      (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons) in
      let (events_in_factual,cf_part) = if inhibitors_cf_indexes = [] then
      begin
        (* If there is no inhibitors (bad heuristic, direct cause blocked), we add direct cause blocked to the core in order to avoid infinite loop *)
        logs "Bad heuristic (no inhibitor) ! Adding direct causes to causal core in order to avoid infinite loop..." ;
        let direct_causes = List.map (function No_reason i -> i | Inhibition _ -> assert false)
        (List.filter (function No_reason _ -> true | Inhibition _ -> false) reasons) in
        (IntSet.union events_in_factual (IntSet.of_list direct_causes), None)
      end
      else begin
        let inhibitors_cf_indexes = choose_arrows inhibitors_cf_indexes config.max_cf_inhibition_arrows in
        let inhibitors_cf = List.map (fun (src,c,dest) -> index_to_id cf_trace src, c, dest) inhibitors_cf_indexes in
        (* Find the inhibitors of cf_eois in the factual trace, and eventually filter them *)
        let cf_eois_indexes = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_cf_indexes in
        let cf_eois = List.map (fun i -> List.nth cf_trace i) (IntSet.elements cf_eois_indexes) in
        let cf_pre_core = if config.precompute_cf_cores
        then Some (compute_causal_core model (cf_grid,cf_vi) (IntSet.elements cf_eois_indexes))
        else None in
        let reasons = List.map (fun e -> find_inhibitive_arrows cf_trace trace (cf_grid,cf_vi) (grid,vi) e cf_pre_core) cf_eois in
        let inhibitors_fc_indexes = List.map (fun reasons -> List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
        (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons)) reasons in
        let inhibitors_fc_indexes = List.map (fun inh -> choose_arrows inh config.max_fc_inhibition_arrows_per_inhibator) inhibitors_fc_indexes in
        let inhibitors_fc_indexes = List.flatten inhibitors_fc_indexes in
        let inhibitors_fc = List.map (fun (src,c,dest) -> src, c, index_to_id cf_trace dest) inhibitors_fc_indexes in
        (* Now we have all inhibition arrows *)
        let inhibitions = inhibitors_fc@inhibitors_cf in
        let cf_indexes_involved = IntSet.union cf_eois_indexes (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_fc_indexes) in
        let f_indexes_involved = IntSet.union (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_cf_indexes)
        (List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_fc_indexes) in
        (* Compute the causal core associated with the cf events involved *)
        logs "Inhibition arrows found ! Computing new causal cores..." ;
        let cf_core = compute_causal_core model (cf_grid,cf_vi) (IntSet.elements cf_indexes_involved) in
        let cf_subtrace = core_to_subtrace cf_trace cf_core in
        (* Retrieving events to add to the factual core : factual events with an inhibitive arrow
        + other factual events of the counterfactual core if we want to have more links with the factual core *)
        let events_in_factual = IntSet.union f_indexes_involved events_in_factual in
        let events_in_factual = if config.add_all_factual_events_involved_to_factual_core
        then IntSet.union (factual_events_of_trace cf_subtrace) events_in_factual
        else events_in_factual in
        (events_in_factual, Some (cf_subtrace,inhibitions))        
      end
      in
      (* Update the factual core *)
      let core = compute_causal_core model (grid,vi) (IntSet.elements events_in_factual) in
      let cf_parts = match cf_part with None -> cf_parts | Some p -> p::cf_parts in
      aux core cf_parts events_in_factual
    )
   in aux core [] (IntSet.singleton (get_index eoi))

let compute_extended_story model ttrace rule_name config : extended_story =
  logs "Computing initial factual core..." ;
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
  let subtrace = core_to_subtrace trace core in
  logs "Extended story complete !" ; (subtrace, cf_parts)
