open Interface
open Ext_tools
open Heuristics

(* TODO : Remove all List.nth and scans from Ext_tools and Compute (use arrays or trace explorer?) *)

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
  max_fc_inhibition_arrows_per_inhibator : int;
  more_relations_with_factual : bool;
  show_entire_counterfactual_stories : bool;
}

let rec get_eoi model rule_name trace = match trace with
  | [] -> raise Not_found
  | s::_ when get_name model s "" = rule_name -> s
  | _::trace -> get_eoi model rule_name trace

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
    | Some i -> aux (List.nth trace1 i)
  and aux dest_event =
    let (tests, _) = grid1.(get_index dest_event) in
    let index2_eq = nb_of_events_before_time_strict trace2 (get_time dest_event 0.0) in
    let inh = List.map (fun c -> (last_inhibitive_event_before index2_eq (grid2,vi2) c, c)) tests in
    let inh = List.filter (fun (opt,c) -> opt <> None) inh in
    let inh = List.map (function (Some src,c) -> (src,c,get_index dest_event) | _ -> assert false) inh in
    match inh with
    | [] -> [No_reason (get_index dest_event)]
    | inh -> List.flatten (List.map rewind inh)
  in aux eoi1

let choose_arrows arrows nb =
  let arrows = List.sort (fun (s,c,d) (s',c',d') -> compare s s') arrows in
  cut_after_index (nb-1) arrows

let factual_events_of_trace trace =
  let steps = List.filter (fun s -> get_id s >= 0) trace in
  IntSet.of_list (List.map get_index steps)

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
    if ratio >= config.threshold then (core, cf_parts)
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
      let follow_core = if config.follow_causal_core then Some core else None in
      let reasons = find_inhibitive_arrows trace cf_trace (grid,vi) (cf_grid,cf_vi) eoi follow_core in
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
        let reasons = List.map (fun e -> find_inhibitive_arrows cf_trace trace (cf_grid,cf_vi) (grid,vi) e None) cf_eois in
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
        let activations = Precedence.compute_strong_deps model cf_grid cf_core in
        let activations = List.map (fun (i1,c,i2) -> (index_to_id cf_trace i1,c,index_to_id cf_trace i2)) activations in
        let precedences = Precedence.transitive_reduction (Precedence.compute_precedence cf_ttrace cf_grid cf_core) in
        let precedences = List.map (fun (i1,i2) -> (index_to_id cf_trace i1,index_to_id cf_trace i2)) precedences in
        (* Retrieving events to add to the factual core : factual events with an inhibitive arrow
        + other factual events of the counterfactual core if we want to have more links with the factual core *)
        let events_in_factual = IntSet.union f_indexes_involved events_in_factual in
        let events_in_factual = if config.more_relations_with_factual
        then IntSet.union (factual_events_of_trace cf_subtrace) events_in_factual
        else events_in_factual in
        (events_in_factual, Some (cf_subtrace,precedences,activations,inhibitions))        
      end
      in
      (* Update the factual core *)
      let core = compute_causal_core model (grid,vi) (IntSet.elements events_in_factual) in
      let cf_parts = match cf_part with None -> cf_parts | Some p -> p::cf_parts in
      aux core cf_parts events_in_factual
    )
   in aux core [] (IntSet.singleton (get_index eoi))

let compute_extended_story model ttrace rule_name config =
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
  (* Merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
      - Merge everything by merging together nodes that represent the same event
      - Keep only counterfactual-only events of counterfactual cores
      Don't forget to put all the inhibition arrows that had been found and to compute relations for the factual core and each counterfactual parts.
  *)
  logs "Finished !" ;
  (* TODO *)
  ()
