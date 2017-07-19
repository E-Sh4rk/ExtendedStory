open Resimulator_interface
open Ext_tools
open Global_trace

type cf_experiment = Global_trace.t * Global_trace.t * ((int * Grid.constr * int) list) * Ext_tools.IntSet.t
(* (factual_subtrace, cf_subtrace, inhibition arrows, blocked events) *)
type extended_story = Global_trace.t * (cf_experiment list) (* (cumulated_factual_subtrace, counterfactual_experiments) *)

type inhibitions_finding_mode = Consider_entire_trace | Prefer_predicted_core | Consider_only_predicted_core
type configuration =
{
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  give_cumulated_core_to_heuristic : bool;
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  trace_scoring_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> int ;
  threshold    : float;
  max_counterfactual_exps : int;
  cf_inhibitions_finding_mode : inhibitions_finding_mode;
  fc_inhibitions_finding_mode : inhibitions_finding_mode;
  max_inhibitors_added_per_factual_events : int;
  max_inhibitors_added_per_cf_events : int;
  add_common_events_to_both_cores : bool;
  compute_inhibition_arrows_for_every_events : bool;
  adjust_inhibition_arrows_with_new_core_predictions : bool;
}

let compress trace eois compression_algorithm =
  if eois = [] then [] else
  (
    let eois = List.sort_uniq Pervasives.compare eois in
    compression_algorithm (get_trace_explorer trace) (get_var_infos trace) eois
  )

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

let activation_event_between trace mode core index1 index2 constr =
  let core_act =
    if mode = Consider_entire_trace then None
    else last_activation_event_between trace core index1 index2 constr in
  if core_act = None && mode <> Consider_only_predicted_core
  then last_activation_event_between trace None index1 index2 constr
  else core_act

type inhibition_reason =
  | Inhibition of int * Grid.constr * int
  | No_reason of int

let is_valid_inhibitor trace1 trace2 i1 =
  if i1 < 0 || i1 >= length trace1 then false
  else if search_global_id trace2 (get_global_id trace1 i1) = None then true
  else false

let find_inhibitive_arrows trace1 trace2 mode core eoi1 =
  let rec rewind (src,c,dest) =
    (*dbg (Format.asprintf "Inh? : %d (%d,%d) -> %d (%d,%d)" src (get_global_id trace2 src) (get_order trace2 src)
    dest (get_global_id trace1 dest) (get_order trace1 dest));*)
    let index1_eq = search_first_after_order trace1 (get_order trace2 src) in
    let index1_eq = match index1_eq with None -> length trace1 | Some i -> i in
    (* If destination is in the core, we can use the core to choose a more relevant reactivator. *)
    let core = match core with
    | None -> None
    | Some core -> if IntSet.mem dest core then Some core else None in
    let act = activation_event_between trace1 mode core (index1_eq-1) dest c in
    (* For the Consider_only_core option, we must be sure that the src is a valid inhibitor. *)
    let act = if act = None && not (is_valid_inhibitor trace2 trace1 src)
    then activation_event_between trace1 mode None (index1_eq-1) dest c else act in
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
  if is_valid_inhibitor trace1 trace2 eoi1 then aux eoi1 else []

let filter_cf_arrows arrows nb =
  let src = List.map (fun (s,_,_) -> s) arrows in
  let src = List.sort_uniq Pervasives.compare src in
  let src = IntSet.of_list (cut_after_index (nb-1) src) in
  List.filter (fun (s,_,_) -> IntSet.mem s src) arrows

let filter_fc_arrows arrows nb = filter_cf_arrows arrows nb

let f_events_indexes_of_cf_core f_trace cf_trace cf_core =
  let f_events = List.filter (fun i -> get_global_id cf_trace i >= 0) cf_core in
  List.map (fun i -> match search_global_id f_trace (get_global_id cf_trace i) with None -> assert false | Some j -> (j,i)) f_events

let cf_events_indexes_of_f_core f_trace cf_trace f_core =
  let cf_events = List.map (fun i -> (i, search_global_id cf_trace (get_global_id f_trace i))) f_core in
  let cf_events = List.filter (function (_,None) -> false | (_,Some _) -> true) cf_events in
  List.map (function (_,None) -> assert false | (i,Some j) -> (i,j)) cf_events

(* Take a list of factual events and a list of cf events,
and return events, inhibitions arrows and directly blocked events that explain all these events. *)
let find_explanations trace cf_trace f_events cf_events predicted_f_core predicted_cf_core config =
  let rec aux f_events cf_events predicted_f_core predicted_cf_core =
    (* We add to the blacklist the events that have been blocked and that are at the origin of the explanations explored. *)
    (* Init case *)
    if IntSet.is_empty f_events && IntSet.is_empty cf_events then (IntSet.empty, IntSet.empty, [], IntSet.empty, IntSet.empty)
    else
    (
      (* For factual events *)
      let predicted_f_core = match predicted_f_core with
      | None -> None
      | Some c -> if IntSet.subset f_events c then Some c else None in
      let predicted_f_core = if predicted_f_core = None && config.cf_inhibitions_finding_mode <> Consider_entire_trace
      then Some (IntSet.of_list (compress trace (IntSet.elements f_events) config.compression_algorithm))
      else predicted_f_core in
      let f_events_lst = IntSet.elements f_events in
      let reasons_f = List.map (fun e -> find_inhibitive_arrows trace cf_trace config.cf_inhibitions_finding_mode predicted_f_core e) f_events_lst in

      let origins_blocked = List.map (function No_reason i -> i | Inhibition _ -> assert false)
        (List.filter (function No_reason _ -> true | Inhibition _ -> false) (List.flatten reasons_f)) in
      let blacklist = IntSet.of_list origins_blocked in

      let directly_blocked = List.map (fun rs -> List.exists (function Inhibition _ -> true | No_reason _ -> false) rs) reasons_f in
      let directly_blocked = List.concat (List.map2 (fun b i -> if b then [] else [i]) directly_blocked f_events_lst) in
      let directly_blocked_ids = IntSet.of_list (List.map (fun i -> get_global_id trace i) directly_blocked) in

      let arrows_cf = List.map (fun lst -> List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
        (List.filter (function No_reason _ -> false | Inhibition _ -> true) lst)) reasons_f in
      let arrows_cf = List.flatten (List.map (fun lst -> filter_cf_arrows lst config.max_inhibitors_added_per_factual_events) arrows_cf) in
      let arrows_cf_ids = List.map (fun (src,c,dest) -> get_global_id cf_trace src, c, get_global_id trace dest) arrows_cf in

      (* For counterfactuals events *)
      let predicted_cf_core = match predicted_cf_core with
      | None -> None
      | Some c -> if IntSet.subset cf_events c then Some c else None in
      let predicted_cf_core = if predicted_cf_core = None && config.fc_inhibitions_finding_mode <> Consider_entire_trace
      then Some (IntSet.of_list (compress cf_trace (IntSet.elements cf_events) config.compression_algorithm))
      else predicted_cf_core in
      let reasons_cf = List.map (fun e -> find_inhibitive_arrows cf_trace trace config.fc_inhibitions_finding_mode predicted_cf_core e) (IntSet.elements cf_events) in

      let arrows_fc = List.map (fun lst -> List.map (function Inhibition (s,c,d) -> (s,c,d) | No_reason _ -> assert false)
        (List.filter (function No_reason _ -> false | Inhibition _ -> true) lst)) reasons_cf in
      let arrows_fc = List.flatten (List.map (fun lst -> filter_fc_arrows lst config.max_inhibitors_added_per_cf_events) arrows_fc) in
      let arrows_fc_ids = List.map (fun (src,c,dest) -> get_global_id trace src, c, get_global_id cf_trace dest) arrows_fc in

      (* Adding events to cores *)
      let cf_eois = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty arrows_cf in
      let f_eois = List.fold_left (fun acc (s,_,_) -> IntSet.add s acc) IntSet.empty arrows_fc in
      let f_events_involved = IntSet.union f_eois (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty arrows_cf) in
      let cf_events_involved = IntSet.union cf_eois (List.fold_left (fun acc (_,_,d) -> IntSet.add d acc) IntSet.empty arrows_fc) in

      (* Recursivity powaaa! *)
      let (f_events_involved_2,cf_events_involved_2,inhibitions_ids_2,directly_blocked_ids_2,blacklist_2) =
        aux f_eois cf_eois predicted_f_core predicted_cf_core in
      let inhibitions_ids = List.sort_uniq Pervasives.compare (arrows_fc_ids@arrows_cf_ids@inhibitions_ids_2) in
      (IntSet.union f_events_involved f_events_involved_2, IntSet.union cf_events_involved cf_events_involved_2, inhibitions_ids, 
      IntSet.union directly_blocked_ids directly_blocked_ids_2,IntSet.union blacklist blacklist_2)
    )
  in aux f_events cf_events (Some (IntSet.of_list predicted_f_core)) (Some (IntSet.of_list predicted_cf_core))

let compute_cores trace cf_trace f_events cf_events config =
  let rec aux f_events cf_events =
    let f_core = compress trace (IntSet.elements f_events) config.compression_algorithm in
    let cf_core = compress cf_trace (IntSet.elements cf_events) config.compression_algorithm in

    let (new_f_events_1,new_cf_events_1) = List.split (f_events_indexes_of_cf_core trace cf_trace cf_core) in
    let (new_f_events_2,new_cf_events_2) = List.split (cf_events_indexes_of_f_core trace cf_trace f_core) in

    let new_f_events = if config.add_common_events_to_both_cores
      then IntSet.union f_events (IntSet.union (IntSet.of_list (new_f_events_1)) (IntSet.of_list (new_f_events_2)))
      else f_events in
    let new_cf_events = if config.add_common_events_to_both_cores
      then IntSet.union cf_events (IntSet.union (IntSet.of_list (new_cf_events_1)) (IntSet.of_list (new_cf_events_2)))
      else cf_events in
    if IntSet.equal f_events new_f_events && IntSet.equal cf_events new_cf_events then (f_events, cf_events, f_core, cf_core)
    else aux new_f_events new_cf_events
  in aux f_events cf_events

let compute_cf_experiment trace cf_trace initial_core eoi config =
  let rec aux f_events cf_events inhibition_arrows blocked blacklist explained_f explained_cf =
    let (f_events, cf_events, f_core, cf_core) = compute_cores trace cf_trace f_events cf_events config in
    let (new_f_events, new_cf_events, new_inhibition_arrows, new_blocked, new_blacklist) =
      if config.compute_inhibition_arrows_for_every_events && config.adjust_inhibition_arrows_with_new_core_predictions
      then find_explanations trace cf_trace (IntSet.of_list f_core) (IntSet.of_list cf_core) f_core cf_core config
      else if config.compute_inhibition_arrows_for_every_events
      then
      (
        let remaining_f = IntSet.diff (IntSet.of_list f_core) explained_f and remaining_cf = IntSet.diff (IntSet.of_list cf_core) explained_cf in
        let (new_f_events, new_cf_events, new_inhibition_arrows, new_blocked, new_blacklist) =
          find_explanations trace cf_trace remaining_f remaining_cf f_core cf_core config in
        (new_f_events, new_cf_events, List.sort_uniq Pervasives.compare (new_inhibition_arrows@inhibition_arrows), IntSet.union blocked new_blocked, new_blacklist)
      )
      else if config.adjust_inhibition_arrows_with_new_core_predictions
      then find_explanations trace cf_trace (IntSet.singleton eoi) IntSet.empty f_core cf_core config
      else (f_events, cf_events, inhibition_arrows, blocked, blacklist) in
    let (new_f_events, new_cf_events, new_blacklist) =
      (IntSet.union f_events new_f_events, IntSet.union cf_events new_cf_events, IntSet.union blacklist new_blacklist) in
    if IntSet.equal f_events new_f_events && IntSet.equal cf_events new_cf_events
    then
    (
      (* We pack it in a counterfactual experiment. *)
      let cf_subtrace = subtrace_of cf_trace cf_core in
      let f_subtrace = subtrace_of trace f_core in
      (Some (f_subtrace,cf_subtrace,new_inhibition_arrows,new_blocked), new_blacklist)
    )
    else aux new_f_events new_cf_events new_inhibition_arrows new_blocked new_blacklist (IntSet.of_list f_core) (IntSet.of_list cf_core)
  in
  let (f_events,cf_events) = (IntSet.singleton eoi, IntSet.empty) in
  let (f_events_2,cf_events_2,inhibition_arrows,blocked,blacklist) =
    find_explanations trace cf_trace f_events cf_events initial_core [] config in
  let (f_events,cf_events) = (IntSet.union f_events f_events_2, IntSet.union cf_events cf_events_2) in
  if List.length inhibition_arrows = 0
  then ( logs ("No inhibition arrow found ! Skipping...") ; (None,blacklist) )
  else aux f_events cf_events inhibition_arrows blocked blacklist (IntSet.singleton eoi) IntSet.empty

let add_cf_experiments trace eoi initial_core config =
  let rec aux cf_exps cumulated_events blacklist =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    logs "Determining interventions (heuristic)..." ;
    let heur_core = if config.give_cumulated_core_to_heuristic
    then compress trace (IntSet.elements cumulated_events) config.compression_algorithm
    else initial_core in
    let interventions = config.heuristic trace blacklist heur_core eoi in
    (*dbg (Format.asprintf "%a" Resimulator_interface.print interventions) ;*)
    let scs = [Event_has_happened eoi;Event_has_not_happened eoi] in

    (* Compute and sample counterfactual traces (resimulation stops when eoi has happened/has been blocked) *)
    (* Take one of the counterfactual traces that failed as witness (heuristic? random among the traces that block the eoi? smallest core?) *)
    logs (Format.asprintf "%a. Resimulating..." Resimulator_interface.print_short interventions) ;
    let (nb_failed,score,wit) = resimulate_and_sample trace eoi initial_core interventions scs config in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    logs ("Ratio S/F : "^(string_of_float ratio)) ;

    if ratio >= config.threshold || nb_failed = 0 || List.length cf_exps >= config.max_counterfactual_exps
    then
    (
      (* We compute the final cumulated factual core *)
      let cumulated_core = compress trace (IntSet.elements cumulated_events) config.compression_algorithm in
      (subtrace_of trace cumulated_core, cf_exps)
    )
    else
    (
      let (trace,cf_trace) = match wit with Some wit -> wit | None -> assert false in
      (*dbg (Format.asprintf "%a" Global_trace.print_full cf_trace) ;*)
      logs ("Resimulation score : "^(string_of_int score)^". Computing the counterfactual part...") ;

      (* Compute the counterfactual experiment *)
      let (cf_exp,blacklist_2) = compute_cf_experiment trace cf_trace initial_core eoi config in
      let blacklist = IntSet.union blacklist blacklist_2 in
      let cumulated_events = match cf_exp with
      | None -> cumulated_events
      | Some (c,_,_,_) -> IntSet.union cumulated_events (IntSet.of_list (List.map (fun i -> get_global_id c i) (n_first_integers (length c)))) in
      let cf_exps = match cf_exp with None -> cf_exps | Some cf_exp -> cf_exp::cf_exps in

      (*dbg (Format.asprintf "%a\n" (fun fmt set -> List.iter (fun i -> Format.fprintf fmt "%d ; " i) (IntSet.elements set)) blacklist) ;*)
      aux cf_exps cumulated_events blacklist
    )
  in aux [] (IntSet.singleton eoi) (IntSet.empty)

let compute_extended_story trace eoi config : extended_story =
  Global_trace.reset_ids ();
  let trace = new_reference_trace trace in
  (*dbg (Format.asprintf "Trace length : %d" (length trace)) ;*)
  (*dbg (Format.asprintf "EOI : %d" eoi) ;*)
  (* Computing factual causal core *)
  logs "Computing initial factual core..." ;
  let core = compress trace [eoi] config.compression_algorithm in
  (*dbg (Format.asprintf "%a" Global_trace.print_full trace) ;*)
  (*dbg (Format.asprintf "Core : %a" (print_core trace) core) ;*)
  (* Adding counterfactual parts *)
  let ext_story = add_cf_experiments trace eoi core config in
  logs "Extended story complete !" ; ext_story

let kaflow_compression tr vi eois =
  Causal_core.core_events (Causal_core.compute_causal_core tr vi eois)
