open Resimulator_interface
open Ext_tools
open Global_trace

type cf_part = Global_trace.t * ((int * Grid.constr * int) list) (* (subtrace, inhibition arrows) *)
type extended_story = Global_trace.t * (cf_part list) (* (subtrace, counterfactual parts) *)

type configuration =
{
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  trace_scoring_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> int ;
  threshold    : float;
  max_counterfactual_parts : int;
  precompute_cores : bool;
  max_cf_inhibition_arrows : int;
  max_fc_inhibition_arrows_per_inhibator : int;
  add_all_factual_events_involved_to_factual_core : bool;
}

let compress trace eois compression_algorithm =
  let eois = List.sort_uniq Pervasives.compare eois in
  compression_algorithm (get_trace_explorer trace) (get_var_infos trace) eois

let cf_trace_succeed eoi trace cf_trace =

  match search_global_id cf_trace (get_global_id trace eoi) with
  | None ->
  (
    let model = get_model trace in
    let name = get_step_name model (get_step trace eoi) "" in
    let cf_index_eq = search_last_before_order cf_trace (get_order trace eoi) in
    let cf_index_eq = match cf_index_eq with None -> -1 | Some i -> i in
    let rec aux i = match i with
      | i when i < 0 -> false
      | i when get_step_name model (get_step cf_trace i) "" = name
      && get_global_id cf_trace i < 0 -> true
      | i -> aux (i-1)
  in aux cf_index_eq
  )
  | Some _ -> true

let resimulate_and_sample trace eoi core block_pred stop_pred config =
  let rec aux n nb_failed score wit = match n with
    | 0 -> (nb_failed,score,wit)
    | n -> let cf_trace = resimulate block_pred stop_pred trace in
    (*dbg (Format.asprintf "%a" Global_trace.print cf_trace) ;*)
    if cf_trace_succeed eoi trace cf_trace
    then aux (n-1) nb_failed score wit
    else
    (
      let new_score = config.trace_scoring_heuristic trace cf_trace core eoi in
      if new_score > score then aux (n-1) (nb_failed+1) new_score (Some (copy trace,cf_trace))
      else aux (n-1) (nb_failed+1) score wit
    )
  in aux config.nb_samples 0 min_int None

let rec last_inhibitive_event_before trace index constr =
  let Grid.Constr (var, _) = constr in
  let history = get_history trace var in
  let last = History.last_before index history in
  match last with
  | None -> Some (-1) (* If there is no event that modified this constraint before,
  we consider that the index -1 is the inhibitor. *)
  | Some i ->
  if List.exists (fun c -> c=constr) (get_actions trace i)
  then None (* It is an activation *)
  else (
      match last_inhibitive_event_before trace i constr with
      | None -> Some i (* If this event created the agent of the constraint,
      we consider it like the inhibitor. *)
      | Some i -> Some i
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
    (*dbg (Format.asprintf "Inh? : %d (%d,%d) -> %d (%d,%d)" src (get_global_id trace2 src) (get_order trace2 src)
    dest (get_global_id trace1 dest) (get_order trace1 dest));*)
    let index1_eq = search_first_after_order trace1 (get_order trace2 src) in
    let index1_eq = match index1_eq with None -> length trace1 | Some i -> i in
    let act = activation_event_between trace1 follow_core (index1_eq-1) dest c in
    match act with
    | None ->
    (*dbg (Format.asprintf "Inh : %d (%d,%d) -> %d (%d,%d)" src (get_global_id trace2 src) (get_order trace2 src)
    dest (get_global_id trace1 dest) (get_order trace1 dest));*)
    assert ((get_global_id trace2 src < 0 && get_global_id trace1 dest >= 0)
         || (get_global_id trace2 src >= 0 && get_global_id trace1 dest < 0));
    [Inhibition (src,c,dest)]
    | Some i ->
    (*dbg (Format.asprintf "Act : %d (%d,%d)" i (get_global_id trace1 i) (get_order trace1 i));*)
    aux i
  and aux dest =
    let index2_eq = search_last_before_order trace2 (get_order trace1 dest) in
    let index2_eq = match index2_eq with None -> -1 | Some i -> i in
    let inh = List.map (fun c -> (last_inhibitive_event_before trace2 (index2_eq+1) c, c)) (get_tests trace1 dest) in
    let inh = List.filter (fun (opt,_) -> opt <> None) inh in
    let inh = List.map (function (Some src,c) -> (src,c,dest) | _ -> assert false) inh in
    match inh with
    | [] -> [No_reason (dest)]
    | inh -> List.flatten (List.map rewind inh)
  in
  match search_global_id trace2 (get_global_id trace1 eoi1) with
  | None -> aux eoi1
  | Some _ -> []

let cmp_inhibition_arrows_earliest (s,c,d) (s',c',d') =
  match Pervasives.compare d d' with
  | 0 -> (match Pervasives.compare s s' with 0 -> Pervasives.compare c c' | n -> n)
  | n -> n

let choose_arrows_cf arrows nb =
  let arrows = List.sort_uniq cmp_inhibition_arrows_earliest arrows in
  cut_after_index (nb-1) arrows

let choose_arrows_fc arrows nb = choose_arrows_cf arrows nb

let factual_events_of_trace trace =
  let rec aux acc i = match i with
  | i when i < 0 -> acc
  | i when get_global_id trace i >= 0 -> aux (IntSet.add i acc) (i-1)
  | i -> aux acc (i-1) in
  aux (IntSet.empty) ((length trace)-1)

let find_cf_part trace cf_trace eoi events_in_factual config =
  let rec aux eoi events_in_factual events_in_cf =
    (* Find the inhibitors of eoi in the cf trace, and eventually filter them. *)
    (* We add to the blacklist the events that have been blocked and that are at the origin of the counterfactual experiment explored. *)
    let events_in_factual = IntSet.add eoi events_in_factual in
    let pre_core = if config.precompute_cores
      then Some (IntSet.of_list (compress trace (IntSet.elements events_in_factual) config.compression_algorithm))
      else None in
    let reasons = find_inhibitive_arrows trace cf_trace pre_core eoi in

    let origins_blocked = List.map (function No_reason i -> i | Inhibition _ -> assert false)
      (List.filter (function No_reason _ -> true | Inhibition _ -> false) reasons) in
    let blacklist = IntSet.of_list origins_blocked in

    let inhibitors_cf = List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
      (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons) in

    if inhibitors_cf = [] then
    begin        
      logs "No inhibition arrow left !" ;
      (events_in_factual, events_in_cf, (*inhibitions_ids*)[], blacklist)
    end
    else begin
      let inhibitors_cf = choose_arrows_cf inhibitors_cf config.max_cf_inhibition_arrows in
      let inhibitors_cf_ids = List.map (fun (src,c,dest) -> get_global_id cf_trace src, c, get_global_id trace dest) inhibitors_cf in
      (* Find the inhibitors of cf_eois in the factual trace, and eventually filter them *)
      let cf_eois = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_cf in
      let events_in_cf = IntSet.union events_in_cf cf_eois in
      let cf_pre_core = if config.precompute_cores
      then Some (IntSet.of_list (compress cf_trace (IntSet.elements events_in_cf) config.compression_algorithm))
      else None in
      let reasons = List.map (fun e -> find_inhibitive_arrows cf_trace trace cf_pre_core e) (IntSet.elements cf_eois) in
      let inhibitors_fc = List.map (fun reasons -> List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
        (List.filter (function No_reason _ -> false | Inhibition _ -> true) reasons)) reasons in
      let inhibitors_fc = List.map (fun inh -> choose_arrows_fc inh config.max_fc_inhibition_arrows_per_inhibator) inhibitors_fc in
      let inhibitors_fc = List.flatten inhibitors_fc in
      let inhibitors_fc_ids = List.map (fun (src,c,dest) -> get_global_id trace src, c, get_global_id cf_trace dest) inhibitors_fc in
      let f_eois = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty inhibitors_fc in
      (* Now we have all inhibition arrows, we retrieve events involved in order to add them in the cores *)
      let inhibitions_ids = inhibitors_fc_ids@inhibitors_cf_ids in
      let cf_involved = IntSet.union cf_eois (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_fc) in
      let f_involved = IntSet.union f_eois (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty inhibitors_cf) in
      (* Recursivity powaaaa ! *)
      let events_in_factual = IntSet.union events_in_factual f_involved in
      let events_in_cf = IntSet.union events_in_cf cf_involved in
      let result = List.map (fun e -> aux e events_in_factual events_in_cf) (IntSet.elements f_eois) in
      List.fold_left (fun (acc1,acc2,acc3,acc4) (e1,e2,e3,e4) -> (IntSet.union acc1 e1, IntSet.union acc2 e2, acc3@e3, IntSet.union acc4 e4))
        (events_in_factual,events_in_cf,List.sort_uniq cmp_inhibition_arrows_earliest inhibitions_ids,blacklist) result
    end
  in aux eoi events_in_factual IntSet.empty

let add_cf_parts trace eoi core config =
  let rec aux core cf_parts events_in_factual blacklist =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    logs "Determining interventions (heuristic)..." ;
    let interventions = config.heuristic trace blacklist core eoi in
    (*dbg (Format.asprintf "%a" Resimulator_interface.print interventions) ;*)
    let scs = [Event_has_happened eoi;Event_has_not_happened eoi] in
    (* Compute and sample counterfactual traces (resimulation stops when eoi has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness (heuristic? random among the traces that block the eoi? smallest core?) *)
    logs (Format.asprintf "%a. Resimulating..." Resimulator_interface.print_short interventions) ;
    let (nb_failed,score,wit) = resimulate_and_sample trace eoi core interventions scs config in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    logs ("Ratio S/F : "^(string_of_float ratio)) ;
    if ratio >= config.threshold || nb_failed = 0 || List.length cf_parts >= config.max_counterfactual_parts then (core, cf_parts)
    else
    (
      let (trace,cf_trace) = match wit with Some wit -> wit | None -> assert false in
      (*dbg (Format.asprintf "%a" Global_trace.print_full cf_trace) ;*)
      logs ("Resimulation score : "^(string_of_int score)^". Computing the counterfactual part...") ;

      (* Compute the counterfactual part *)
      let (events_in_factual,events_in_cf,inhibitions_ids,blacklist2) = find_cf_part trace cf_trace eoi events_in_factual config in
      let blacklist = IntSet.union blacklist blacklist2 in
      let (cf_part,events_in_factual) = if List.length inhibitions_ids = 0 then 
      (
        logs ("No inhibition found ! Skipping...") ;
        (None,events_in_factual)
      )
      else
      (
        (* Compute the counterfactual causal core *)
        logs ((string_of_int (List.length inhibitions_ids))^" inhibition arrows found ! Computing new causal cores...") ;
        let cf_core = compress cf_trace (IntSet.elements events_in_cf) config.compression_algorithm in
        let cf_subtrace = subtrace_of cf_trace cf_core in
        let events_in_factual = if config.add_all_factual_events_involved_to_factual_core
        then IntSet.union (factual_events_of_trace cf_subtrace) events_in_factual else events_in_factual in
        (Some (cf_subtrace,inhibitions_ids), events_in_factual)
      ) in
      let cf_parts = match cf_part with None -> cf_parts | Some cfp -> cfp::cf_parts in
      (* Update the factual core *)
      let core = compress trace (IntSet.elements events_in_factual) config.compression_algorithm in

      (*dbg (Format.asprintf "%a\n" (fun fmt set -> List.iter (fun i -> Format.fprintf fmt "%d ; " i) (IntSet.elements set)) blacklist) ;*)
      aux core cf_parts events_in_factual blacklist
    )
  in aux core [] (IntSet.singleton eoi) (IntSet.empty)

let compute_extended_story trace eoi config : extended_story =
  Global_trace.reset_ids ();
  let trace = new_reference_trace trace in
  (*dbg (Format.asprintf "Trace length : %d" (length trace)) ;*)
  (*dbg (Format.asprintf "EOI : %d" eoi) ;*)
  (* Computing factual causal core *)
  logs "Computing initial factual core..." ;
  let core = compress trace [eoi] config.compression_algorithm in
  (*dbg (Format.asprintf "Core : %a" (print_core trace) core) ;*)
  (* Adding counterfactual parts *)
  let (core, cf_parts) = add_cf_parts trace eoi core config in
  let subtrace = subtrace_of trace core in
  logs "Extended story complete !" ; (subtrace, cf_parts)

let kaflow_compression tr vi eois =
  Causal_core.core_events (Causal_core.compute_causal_core tr vi eois)
