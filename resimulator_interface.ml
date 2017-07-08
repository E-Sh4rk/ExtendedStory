open Ext_tools
open Resimulation
open Trace

type step = Trace.step
type counterfactual_step = Resimulation.step

type blocked_f_event = int (* index *)

type blocked_cf_event =
  | Blocked_rule of int * ASet.t * int option * int option (* rule_id * agents_modified (subset) * after_f_event * before_f_event *)
  | Blocked_step of ASet.t * int option * int option (* agents_modified (subset) * after_f_event * before_f_event *)

type interventions = blocked_f_event list * blocked_cf_event list

type stop_condition =
  | Time_limit of float
  | Event_has_happened of int (* index *)
  | Event_has_not_happened of int (* index *)
  | Rule_has_happened of int (* rule_id *)
  | Rule_has_not_happened of int (* rule_id *)
  | Obs_has_happened of string
  | Obs_has_not_happened of string
  | Any_event_not_happened

type stop_conditions = stop_condition list

type stop =
  | Continue | Stop_after | Stop_before

let is_compatible (agents,after,before) last_f_event actions =
  let after = match after with None -> last_f_event | Some i -> i in
  let before = match before with None -> last_f_event+1 | Some i -> i in
  if last_f_event < after || last_f_event >= before then false
  else ASet.subset agents (agents_modified actions)

let is_cf_event_blocked cf last_f_event _ rid_opt actions =
  let rec aux cf =
  match cf with
  | [] -> false
  | (Blocked_rule (rid,agents,after,before))::cf ->
  (
    match rid_opt with
    | Some rid' -> if rid' = rid && is_compatible (agents,after,before) last_f_event actions
    then true else aux cf
    | None -> aux cf
  )
  | (Blocked_step (agents,before,after))::cf ->
  (
    if is_compatible (agents,after,before) last_f_event actions
    then true else aux cf
  )
  in aux cf

let rec must_stop scs last_f_event cstep =
  match scs, cstep with
  | [], _ -> Continue
  | (Time_limit t)::lst, Factual_happened step
  | (Time_limit t)::lst, Factual_did_not_happen (_, step)
  | (Time_limit t)::lst, Counterfactual_happened step ->
  ( if get_time_of_step step 0.0 > t then Stop_before else must_stop lst last_f_event cstep )
  | (Any_event_not_happened)::_, Factual_did_not_happen _ -> Stop_after
  | (Event_has_not_happened ev)::lst, Factual_did_not_happen (_, _)
  | (Event_has_happened ev)::lst, Factual_happened _ ->
  ( if last_f_event = ev then Stop_after else must_stop lst last_f_event cstep )
  | (Rule_has_not_happened rid)::lst, Factual_did_not_happen (_, step)
  | (Rule_has_happened rid)::lst, Factual_happened step
  | (Rule_has_happened rid)::lst, Counterfactual_happened step ->
  (
    match step with
    | Rule(rid',_,_) when rid = rid' -> Stop_after
    | _ -> must_stop lst last_f_event cstep
  )
  | (Obs_has_not_happened name)::lst, Factual_did_not_happen (_, step)
  | (Obs_has_happened name)::lst, Factual_happened step
  | (Obs_has_happened name)::lst, Counterfactual_happened step ->
  (
    match step with
    | Obs(name',_,_) when name = name' -> Stop_after
    | _ -> must_stop lst last_f_event cstep
  )
  | _::lst, cstep -> must_stop lst last_f_event cstep

let indexes_involved b_cf_event = match b_cf_event with
  | Blocked_rule (_,_,None,None) -> [0]
  | Blocked_rule (_,_,Some i,None) | Blocked_rule (_,_,None,Some i) -> [0;i]
  | Blocked_rule (_,_,Some i1,Some i2) -> [i1;i2]
  | Blocked_step (_,None,None) -> [0]
  | Blocked_step (_,Some i,None) | Blocked_step (_,None,Some i) -> [0;i]
  | Blocked_step (_,Some i1,Some i2) -> [i1;i2]

let random_state = Random.State.make_self_init ()

let resimulate (b_f,b_cf) scs trace =
  let b_cf_indexes = List.flatten (List.map indexes_involved b_cf) in
  let b_cf_indexes = IntSet.of_list b_cf_indexes in
  let b_f = IntSet.of_list b_f in
  let next_event i = match i with
  | i when i >= Global_trace.length trace -> None
  | i -> Some (Global_trace.get_step trace i, IntSet.mem i b_f)
  in
  let last_next_index = ref (-1) in
  let rec resimulate_step next_event_index state acc =
    let state = if !last_next_index <> next_event_index then
    (
      last_next_index := next_event_index ;
      if IntSet.mem (next_event_index-1) b_cf_indexes
      then Resimulation.set_events_to_block (Some (is_cf_event_blocked b_cf (next_event_index-1))) state
      else state
    ) else state in
    let (consummed, cstep_opt, state) = Resimulation.do_step (next_event next_event_index) state in
    let next_event_index = if consummed then next_event_index + 1 else next_event_index in
    try
    (
      match cstep_opt with
      | None -> resimulate_step next_event_index state acc
      | Some s when must_stop scs (next_event_index-1) s = Continue -> resimulate_step next_event_index state (Global_trace.add_counterfactual_step trace acc s)
      | Some s when must_stop scs (next_event_index-1) s = Stop_after -> Global_trace.add_counterfactual_step trace acc s
      | Some _ -> acc
    )
    with Resimulation.End_of_resimulation -> acc
  in
  let builder = Global_trace.new_counterfactual_trace_builder () in
  let builder = resimulate_step 0 (Resimulation.init (Global_trace.get_model trace) random_state) builder in
  Global_trace.finalize_counterfactual_trace trace builder

let print fmt (f,cf) =
  List.iter (fun x -> Format.fprintf fmt "%d ; " x) f ;
  if cf <> [] then Format.fprintf fmt " (+cf)"

let print_short fmt (f,cf) =
  Format.fprintf fmt "%d events blocked" (List.length f) ;
  if cf <> [] then Format.fprintf fmt " (+cf)";
  Format.fprintf fmt "."
