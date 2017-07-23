open Resimulator_interface
open Ext_tools
open Global_trace

type cf_experiment = Global_trace.t * Global_trace.t * Ext_tools.InhSet.t * Ext_tools.IntSet.t
(* (factual subtrace, cf subtrace, inhibition arrows, blocked events) *)
type extended_story = Global_trace.t * Global_trace.t * Global_trace.t * (cf_experiment list)
(* (initial subtrace, extended subtrace, cumulative subtrace, experiments) *)

type inhibitions_finding_mode = Consider_entire_trace | Prefer_predicted_core | Consider_only_predicted_core
type activation_paths_mode = Do_not_minimize | Minimize | Do_not_impose_activation_path
type configuration =
{
  compression_algorithm : Trace_explorer.t -> Causal_core.var_info_table -> int list -> int list;
  give_cumulative_core_to_heuristic : bool;
  heuristic    : Global_trace.t -> Ext_tools.IntSet.t -> int list -> int -> Resimulator_interface.interventions;
  nb_samples   : int;
  trace_scoring_heuristic : Global_trace.t -> Global_trace.t -> int list -> int -> int ;
  threshold    : float;
  max_counterfactual_exps : int;
  cf_inhibitions_finding_mode : inhibitions_finding_mode;
  cf_activation_paths_compression : activation_paths_mode;
  fc_inhibitions_finding_mode : inhibitions_finding_mode;
  fc_activation_paths_compression : activation_paths_mode;
  max_inhibitors_added_per_factual_events : int;
  max_inhibitors_added_per_cf_events : int;
  include_initial_story_in_experiments : bool;
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
  let prefered_act =
    if mode = Consider_entire_trace then None
    else last_activation_event_between trace core index1 index2 constr in
  if prefered_act = None && mode <> Consider_only_predicted_core
  then last_activation_event_between trace None index1 index2 constr
  else prefered_act

type inhibition_reason =
  | Inhibition of (int * Grid.constr * int) * IntSet.t (* inhibition arrow, events involved *)
  | Blocked of int * IntSet.t (* event blocked, events involved *)

let is_valid_inhibitor trace1 trace2 i1 =
  if i1 < 0 || i1 >= length trace1 then false
  else if search_global_id trace2 (get_global_id trace1 i1) = None then true
  else false

(* Take a path of activations and minimize it.
The set of events returned is a subset of the initial path that is also an activation path. *)
let minimize_activation_path trace events core =
  let rec aux events =
    let rec cut_after_first_that_has_relation lst c acc = match lst with
    | [] -> assert false
    | i::_ when not (ConstrSet.is_empty (ConstrSet.inter (ConstrSet.of_list (get_tests trace i)) c)) -> i::acc
    | x::lst -> cut_after_first_that_has_relation lst c (x::acc)
    in
    let cut_until_first_in_core_that_has_relation lst c =
      let rec aux2 l acc = match l with
      | [] -> cut_after_first_that_has_relation lst c []
      | i::_ when IntSet.mem i core &&
      not (ConstrSet.is_empty (ConstrSet.inter (ConstrSet.of_list (get_tests trace i)) c)) -> i::acc
      | x::l -> aux2 l (x::acc)
      in aux2 lst []
    in
    if List.length events <= 2 then IntSet.of_list events
    else
    (
      let src = List.hd events in
      let src_actions = ConstrSet.diff (ConstrSet.of_list (get_actions trace src)) (ConstrSet.of_list (get_tests trace src)) in
      let events = List.rev (List.tl events) in
      let events = cut_until_first_in_core_that_has_relation events src_actions in
      IntSet.add src (aux events)
    )
  in aux events

let events_involved_in_activation trace events core act_mode = match act_mode with
  | Do_not_impose_activation_path -> IntSet.of_list [List.hd events ; List.hd (List.rev events)]
  | Minimize -> minimize_activation_path trace events core
  | Do_not_minimize -> IntSet.of_list events

(* Return the inhibitive arrows and the events involved for an event that happened only in trace1. *)
let find_inhibitive_arrows trace1 trace2 find_mode act_mode core eoi1 =
  let rec rewind involved (src,c,dest) =
    (*dbg (Format.asprintf "Inh? : %d (%d,%d) -> %d (%d,%d)" src (get_global_id trace2 src) (get_order trace2 src)
    dest (get_global_id trace1 dest) (get_order trace1 dest));*)
    let index1_eq = search_first_after_order trace1 (get_order trace2 src) in
    let index1_eq = match index1_eq with None -> length trace1 | Some i -> i in
    (* If destination is in the core, we can use the core to choose a more relevant reactivator. *)
    let act_core = if IntSet.mem dest core then Some core else None in
    let act = activation_event_between trace1 find_mode act_core (index1_eq-1) dest c in
    (* For the Consider_only_core option, we must be sure that the src is a valid inhibitor. *)
    let act = if act = None && not (is_valid_inhibitor trace2 trace1 src)
    then activation_event_between trace1 find_mode None (index1_eq-1) dest c else act in
    match act with
    | None ->
    (*dbg (Format.asprintf "Inh : %d (%d,%d) -> %d (%d,%d)" src (get_global_id trace2 src) (get_order trace2 src)
    dest (get_global_id trace1 dest) (get_order trace1 dest));*)
    assert ((get_global_id trace2 src < 0 && get_global_id trace1 dest >= 0)
         || (get_global_id trace2 src >= 0 && get_global_id trace1 dest < 0));
    [Inhibition ((src,c,dest), events_involved_in_activation trace1 involved core act_mode)]
    | Some i ->
    (*dbg (Format.asprintf "Act : %d (%d,%d)" i (get_global_id trace1 i) (get_order trace1 i));*)
    aux involved i
  and aux involved dest =
    let involved = dest::involved in
    let index2_eq = search_last_before_order trace2 (get_order trace1 dest) in
    let index2_eq = match index2_eq with None -> -1 | Some i -> i in
    let inh = List.map (fun c -> (last_inhibitive_event_before trace2 (index2_eq+1) c, c)) (get_tests trace1 dest) in
    let inh = List.filter (fun (opt,_) -> opt <> None) inh in
    let inh = List.map (function (Some src,c) -> (src,c,dest) | _ -> assert false) inh in
    match inh with
    | [] -> [Blocked (dest,events_involved_in_activation trace1 involved core act_mode)]
    | inh -> List.flatten (List.map (fun (src,c,dest) -> rewind involved (src,c,dest)) inh)
  in
  if is_valid_inhibitor trace1 trace2 eoi1 then aux [] eoi1 else []

let filter_cf_arrows arrows nb =
  let src = List.map (fun ((s,_,_),_) -> s) arrows in
  let src = List.sort_uniq Pervasives.compare src in
  let src = IntSet.of_list (cut_after_index (nb-1) src) in
  List.filter (fun ((s,_,_),_) -> IntSet.mem s src) arrows

let filter_fc_arrows arrows nb = filter_cf_arrows arrows nb

let f_events_indexes_of_cf_core f_trace cf_trace cf_core =
  let f_events = List.filter (fun i -> get_global_id cf_trace i >= 0) cf_core in
  List.map (fun i -> match search_global_id f_trace (get_global_id cf_trace i) with None -> assert false | Some j -> (j,i)) f_events

let cf_events_indexes_of_f_core f_trace cf_trace f_core =
  let cf_events = List.map (fun i -> (i, search_global_id cf_trace (get_global_id f_trace i))) f_core in
  let cf_events = List.filter (function (_,None) -> false | (_,Some _) -> true) cf_events in
  List.map (function (_,None) -> assert false | (i,Some j) -> (i,j)) cf_events

(* Take a list of initial factual events and a list of initial cf events,
and return all events, inhibitions arrows and directly blocked events necessary to explain the eventual absence of these initial events from the other trace. *)
let find_explanations trace cf_trace f_events cf_events predicted_f_core predicted_cf_core config =
  let rec aux f_events cf_events predicted_f_core predicted_cf_core =
    (* Init case *)
    if IntSet.is_empty f_events && IntSet.is_empty cf_events then (IntSet.empty, IntSet.empty, InhSet.empty, IntSet.empty)
    else
    (
      (* Updating predicted cores *)
      let predicted_f_core =  if IntSet.subset f_events predicted_f_core then predicted_f_core
      else IntSet.of_list (compress trace (IntSet.elements (IntSet.union f_events predicted_f_core)) config.compression_algorithm) in

      let predicted_cf_core = if IntSet.subset cf_events predicted_cf_core then predicted_cf_core
      else IntSet.of_list (compress cf_trace (IntSet.elements (IntSet.union cf_events predicted_cf_core)) config.compression_algorithm) in

      (* For factual events *)
      let reasons_f = List.map
      (fun e -> find_inhibitive_arrows trace cf_trace config.cf_inhibitions_finding_mode config.cf_activation_paths_compression predicted_f_core e)
      (IntSet.elements f_events) in

      (* If there is no indirect reason, we search a direct reason. *)
      let no_indirect_reason = List.filter (fun rs -> not (List.exists (function Inhibition _ -> true | Blocked _ -> false) rs)) reasons_f in
      let no_indirect_reason = List.map (function Inhibition _ -> assert false | Blocked (b,inv) -> (b,inv)) (List.flatten no_indirect_reason) in
      let origins_blocked = List.map (fun (b,_) -> b) no_indirect_reason in
      let origins_blocked_ids = IntSet.of_list (List.map (fun i -> get_global_id trace i) origins_blocked) in
      let f_events_involved = List.fold_left (fun acc (_,inv) -> IntSet.union acc inv) IntSet.empty no_indirect_reason in

      let arrows_cf = List.map (fun lst -> List.map (function Inhibition (arrow,inv) -> (arrow,inv) | Blocked _ -> assert false)
        (List.filter (function Blocked _ -> false | Inhibition _ -> true) lst)) reasons_f in
      let arrows_cf = List.flatten (List.map (fun lst -> filter_cf_arrows lst config.max_inhibitors_added_per_factual_events) arrows_cf) in
      let arrows_cf_ids = InhSet.of_list (List.map (fun ((src,c,dest),_) -> get_global_id cf_trace src, c, get_global_id trace dest) arrows_cf) in
      let f_events_involved = IntSet.union f_events_involved (List.fold_left (fun acc (_,inv) -> IntSet.union acc inv) IntSet.empty arrows_cf) in
      let cf_eois = List.fold_left (fun acc ((s,_,_),_) -> IntSet.add s acc) IntSet.empty arrows_cf in

      (* For counterfactuals events *)
      let reasons_cf = List.map
      (fun e -> find_inhibitive_arrows cf_trace trace config.fc_inhibitions_finding_mode config.fc_activation_paths_compression predicted_cf_core e)
      (IntSet.elements cf_events) in

      let arrows_fc = List.map (fun lst -> List.map (function Inhibition (arrow,inv) -> (arrow,inv) | Blocked _ -> assert false)
        (List.filter (function Blocked _ -> false | Inhibition _ -> true) lst)) reasons_cf in
      let arrows_fc = List.flatten (List.map (fun lst -> filter_fc_arrows lst config.max_inhibitors_added_per_cf_events) arrows_fc) in
      let arrows_fc_ids = InhSet.of_list (List.map (fun ((src,c,dest),_) -> get_global_id trace src, c, get_global_id cf_trace dest) arrows_fc) in
      let cf_events_involved = List.fold_left (fun acc (_,inv) -> IntSet.union acc inv) IntSet.empty arrows_fc in
      let f_eois = List.fold_left (fun acc ((s,_,_),_) -> IntSet.add s acc) IntSet.empty arrows_fc in

      (* Recursivity powaaa! *)
      let (f_events_involved_2,cf_events_involved_2,inhibitions_ids_2,origins_blocked_ids_2) =
        aux f_eois cf_eois predicted_f_core predicted_cf_core in
      (IntSet.union f_events_involved f_events_involved_2, IntSet.union cf_events_involved cf_events_involved_2,
      InhSet.union (InhSet.union arrows_fc_ids arrows_cf_ids) inhibitions_ids_2, IntSet.union origins_blocked_ids origins_blocked_ids_2)
    )
  in aux f_events cf_events (IntSet.of_list predicted_f_core) (IntSet.of_list predicted_cf_core)

let compute_cores trace cf_trace f_events cf_events config =
  let rec aux f_events cf_events =
    let f_core = compress trace (IntSet.elements f_events) config.compression_algorithm in
    let cf_core = compress cf_trace (IntSet.elements cf_events) config.compression_algorithm in
    (* Adding common events *)
    let (new_f_events_1,new_cf_events_1) = List.split (f_events_indexes_of_cf_core trace cf_trace cf_core) in
    let (new_f_events_2,new_cf_events_2) = List.split (cf_events_indexes_of_f_core trace cf_trace f_core) in

    let f_events = if config.add_common_events_to_both_cores
      then IntSet.union f_events (IntSet.union (IntSet.of_list (new_f_events_1)) (IntSet.of_list (new_f_events_2)))
      else f_events in
    let cf_events = if config.add_common_events_to_both_cores
      then IntSet.union cf_events (IntSet.union (IntSet.of_list (new_cf_events_1)) (IntSet.of_list (new_cf_events_2)))
      else cf_events in
    (* Fixpoint search *)
    if IntSet.subset f_events (IntSet.of_list f_core) && IntSet.subset cf_events (IntSet.of_list cf_core)
    then (f_events, cf_events, f_core, cf_core)
    else aux f_events cf_events
  in aux f_events cf_events

let compute_cf_experiment trace cf_trace initial_core eoi config =
  let rec aux f_events cf_events inhibition_arrows blocked explained_f explained_cf cumulative_f_events cumulative_cf_events =
    (* To prevent infinite loops *)
    let (f_events, cf_events) = if IntSet.subset f_events cumulative_f_events && IntSet.subset cf_events cumulative_cf_events
    then (cumulative_f_events, cumulative_cf_events) else (f_events, cf_events) in
    let (cumulative_f_events, cumulative_cf_events) =
      (IntSet.union cumulative_f_events f_events, IntSet.union cumulative_cf_events cf_events) in
    let (f_events, cf_events, f_core, cf_core) = compute_cores trace cf_trace f_events cf_events config in
    (* Adding explanations (inhibition arrows and events involved) *)
    let (f_to_explains, cf_to_explains) = if config.compute_inhibition_arrows_for_every_events
    then (IntSet.of_list f_core, IntSet.of_list cf_core) else (IntSet.singleton eoi, IntSet.empty) in
    let (f_events, cf_events, inhibition_arrows, blocked) =
      if config.adjust_inhibition_arrows_with_new_core_predictions
      then find_explanations trace cf_trace f_to_explains cf_to_explains f_core cf_core config
      else
      (
        let remaining_f = IntSet.diff f_to_explains explained_f and remaining_cf = IntSet.diff cf_to_explains explained_cf in
        let (new_f_events, new_cf_events, new_inhibition_arrows, new_blocked) =
          find_explanations trace cf_trace remaining_f remaining_cf f_core cf_core config in
        (IntSet.union f_events new_f_events, IntSet.union cf_events new_cf_events,
        InhSet.union new_inhibition_arrows inhibition_arrows, IntSet.union blocked new_blocked)
      ) in
    (* Fixpoint search *)
    if IntSet.subset f_events (IntSet.of_list f_core) && IntSet.subset cf_events (IntSet.of_list cf_core)
    then
    (
      (* We pack it in a counterfactual experiment. *)
      if InhSet.cardinal inhibition_arrows = 0 then ( logs ("No inhibition arrow found ! Skipping...") ; (None,blocked) )
      else
      (
        let cf_subtrace = subtrace_of cf_trace cf_core in
        let f_subtrace = subtrace_of trace f_core in
        (Some (f_subtrace,cf_subtrace,inhibition_arrows,blocked),blocked)
      )
    )
    else aux f_events cf_events inhibition_arrows blocked f_to_explains cf_to_explains cumulative_f_events cumulative_cf_events
  in
  let f_events = if config.include_initial_story_in_experiments then IntSet.of_list initial_core else IntSet.singleton eoi in
  aux f_events IntSet.empty InhSet.empty IntSet.empty IntSet.empty IntSet.empty IntSet.empty IntSet.empty

let add_cf_experiments trace eoi initial_core config =
  let rec aux cf_exps cumulative_events blacklist =
    (* Choose intervention (heuristic) depending on the trace and the current factual causal core. *)
    logs "Determining interventions (heuristic)..." ;
    let heur_core = if config.give_cumulative_core_to_heuristic
    then compress trace (IntSet.elements cumulative_events) config.compression_algorithm
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
      (* We compute the final cumulative factual core and the extended core *)
      let cumulative_core = compress trace (IntSet.elements cumulative_events) config.compression_algorithm in
      let cumulative_subtrace = subtrace_of trace cumulative_core in
      let ext_events = if config.include_initial_story_in_experiments then IntSet.of_list initial_core else IntSet.singleton eoi in
      let ext_events = IntSet.map (fun i -> get_global_id trace i) ext_events in
      let ext_events = List.fold_left (fun acc (_,_,_,lst) -> IntSet.union acc lst) ext_events cf_exps in
      let ext_events = IntSet.map (fun id -> match search_global_id cumulative_subtrace id with None -> assert false | Some i -> i) ext_events in
      let extended_core = compress cumulative_subtrace (IntSet.elements ext_events) config.compression_algorithm in
      (subtrace_of trace initial_core, subtrace_of cumulative_subtrace extended_core, cumulative_subtrace, cf_exps)
    )
    else
    (
      let (trace,cf_trace) = match wit with Some wit -> wit | None -> assert false in
      (*dbg (Format.asprintf "%a" Global_trace.print_full cf_trace) ;*)
      logs ("Resimulation score : "^(string_of_int score)^". Computing the counterfactual part...") ;

      (* Compute the counterfactual experiment *)
      let (cf_exp,blocked) = compute_cf_experiment trace cf_trace initial_core eoi config in
      (* /!\ In the 2 following statements, we assimilate ID and index because it is coincide for the factual trace. *)
      let blacklist = IntSet.union blacklist blocked in
      let cumulated_events = match cf_exp with
      | None -> cumulative_events
      | Some (c,_,_,_) -> IntSet.union cumulative_events (IntSet.of_list (List.map (fun i -> get_global_id c i) (n_first_integers (length c)))) in
      let cf_exps = match cf_exp with None -> cf_exps | Some cf_exp -> cf_exp::cf_exps in

      (*dbg (Format.asprintf "%a\n" (fun fmt set -> List.iter (fun i -> Format.fprintf fmt "%d ; " i) (IntSet.elements set)) blacklist) ;*)
      aux cf_exps cumulated_events blacklist
    )
  
  in aux [] (IntSet.of_list initial_core) (IntSet.empty)

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
