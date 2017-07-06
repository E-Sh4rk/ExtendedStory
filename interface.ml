open Ext_tools
open Resimulation
open Trace

type step = Trace.step
type counterfactual_step = Resimulation.step

type blocked_f_event = int (* index *)

type blocked_cf_event =
  | Blocked_rule of int * ASet.t * int option * int option (* rule_id * agents_involved (subset) * after_f_event * before_f_event *)
  | Blocked_pert of string * ASet.t * int option * int option (* pert_name * agents_involved (subset) * after_f_event * before_f_event *)

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

let is_compatible (agents,after,before) last_f_event inst =
  let after = match after with None -> last_f_event | Some i -> i in
  let before = match before with None -> last_f_event+1 | Some i -> i in
  if last_f_event < after || last_f_event >= before then false
  else ASet.subset agents (agents_tested inst.Instantiation.tests)

let rec is_cf_event_blocked (f,cf) last_f_event step =
  match cf with
  | [] -> false
  | (Blocked_rule (rid,agents,after,before))::cf ->
  (
    match step with
    | Trace.Rule (rid', inst, _) -> if rid' = rid && is_compatible (agents,after,before) last_f_event inst
    then true else is_cf_event_blocked (f,cf) last_f_event step
    | _ -> is_cf_event_blocked (f,cf) last_f_event step
  )
  | (Blocked_pert (name,agents,before,after))::cf ->
  (
    match step with
    | Trace.Pert (name', inst, _) -> if name' = name && is_compatible (agents,after,before) last_f_event inst
    then true else is_cf_event_blocked (f,cf) last_f_event step
    | _ -> is_cf_event_blocked (f,cf) last_f_event step
  )

let rec must_stop scs last_f_event cstep =
  match scs, cstep with
  | [], _ -> Continue
  | (Time_limit t)::lst, Factual_happened step
  | (Time_limit t)::lst, Factual_did_not_happen (_, step)
  | (Time_limit t)::lst, Counterfactual_happened step ->
  ( if get_time_ts step 0.0 > t then Stop_before else must_stop lst last_f_event cstep )
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

(* Do not handle counterfactual events blocking for now. Waiting for a resimulator update. *)
let resimulate (b_f,b_cf) scs trace =
  let b_f = List.sort_uniq Pervasives.compare b_f in
  let next_blocked = ref b_f in
  let last_time = ref 0.0 in
  let next_event i = match i with
  | i when i >= Global_trace.length trace -> None
  | i ->
  let (blocked,next_b) = ( match !next_blocked with b::next_b when b=i -> (true,next_b) | next_b -> (false,next_b) ) in
  next_blocked := next_b ; let s = Global_trace.get_step trace i in
  last_time := get_time_ts s (!last_time) ; Some (s, !last_time, blocked)
  in
  let rec resimulate_step next_event_index state acc =
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
  let builder = resimulate_step 0 (Resimulation.init (Global_trace.get_model trace) (Random.get_state ())) builder in
  Global_trace.finalize_counterfactual_trace trace builder
