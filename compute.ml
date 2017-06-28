open Interface
open Trace
open Ext_tools

type configuration =
{
  nb_samples   : int;
  threshold    : float;
  more_inhibition_arrows : bool;
  more_relations_with_factual : bool;
  show_entire_counterfactual_stories : bool;
}

let heuristic_choose_interventions () : interventions = []

exception Not_found

let rec get_first_rule_event model rule_name trace = match trace with
  | [] -> raise Not_found
  | (Rule (rule_id,inst,infos))::trace when rule_ast_name model rule_id = rule_name -> infos.story_id
  | (Obs (name,inst,infos))::trace when name = rule_name -> infos.story_id
  | s::trace -> get_first_rule_event model rule_name trace

let trace_succeed eoi_id trace = match List.length trace with
  | 0 -> false
  | n -> begin match List.nth trace (n-1) with
    | Resimulation.Factual_happened (Rule (rid,inst,infos)) when infos.story_id = eoi_id -> true
    | _ -> false
  end

let resimulate_and_sample nb eoi_id model block_pred stop_pred trace =
  let rec aux nb acc = match nb with
    | 0 -> acc
    | n -> let (nb_failed,wit) = acc in 
    let ctrace = resimulate model block_pred stop_pred trace in
    if trace_succeed eoi_id ctrace then
    aux (nb-1) (nb_failed,wit)
    else
    aux (nb-1) (nb_failed+1,Some ctrace)
  in aux nb (0,None)

let compute_causal_core model trace eoi_id = []

let set_ids_of_counterfactual_trace trace =
  let rec aux trace next_id = match trace with
  | [] -> []
  | (Resimulation.Counterfactual_happened step)::trace -> (Resimulation.Counterfactual_happened (set_id next_id step))::(aux trace (next_id-1))
  | (Resimulation.Factual_happened step)::trace -> (Resimulation.Factual_happened step)::(aux trace (next_id(*-1*)))
  | (Resimulation.Factual_did_not_happen (b,step))::trace -> (Resimulation.Factual_did_not_happen (b,step))::(aux trace next_id)
  in aux trace (-1)
 
let counterfactual_trace_to_regular trace =
  let is_happenning_event cs = match cs with
  | Resimulation.Factual_happened _ | Resimulation.Counterfactual_happened _ -> true
  | Resimulation.Factual_did_not_happen _ -> false in
  let regularize_event cs = match cs with
  | Resimulation.Factual_happened step -> step
  | Resimulation.Counterfactual_happened step -> step
  | Resimulation.Factual_did_not_happen _ -> failwith "Invalid counterfactual trace !" in
  let trace = List.filter is_happenning_event trace in
  List.map regularize_event trace

let rec first_inhibited_event factual_subtrace ctrace = match ctrace with
  | [] -> failwith "First inhibited event not found !"
  | (Resimulation.Factual_did_not_happen (blocked, step))::ctrace when not blocked
  -> let id = get_id step in
  if id <> 0 && get_event id factual_subtrace <> None
  then step else first_inhibited_event factual_subtrace ctrace
  | _::ctrace -> first_inhibited_event factual_subtrace ctrace

let last_inhibitive_event_before index grid constr = ()

type counterfactual_part = (step list) * ((int*int) list)
(*
(events * inhibition arrows) 
IDs of the events are :
 > 0 for factual events 
 < 0 for counterfactual-only events *)

 let add_counterfactual_parts model trace eoi_id config factual =
   (* Factual subtrace, counterfactual parts, events to maintain in the factual subtrace *)
   let rec aux factual counterfactuals events_in_story =
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
    let (nb_failed,ctrace) = resimulate_and_sample config.nb_samples eoi_id model block_pred stop_pred trace in
    let ratio = 1.0 -. (float_of_int nb_failed)/.(float_of_int config.nb_samples) in
    if ratio >= config.threshold then (factual, counterfactuals)
    else
    (
      (* Don't forget to set IDs for counterfactual events *)
      let Some ctrace = ctrace in
      let ctrace = set_ids_of_counterfactual_trace ctrace in
      (* Convert the counterfactual trace to a regular trace *)
      let reg_ctrace = counterfactual_trace_to_regular ctrace in
      (* Find the last events that has inhibited the first event of the causal core that has been inhibited :
      it is the last events that changed the value of a tested logical site from a good value to a wrong value. *)
      let inhibited_event = first_inhibited_event factual ctrace in
      let ctrace_array = Array.of_list reg_ctrace in
      let (ctrace_grid, _) = Grid.build_grid model reg_ctrace in

      (* Select the first (earliest) of these events and compute its causal core. Add this counterfactual causal core to the list and indicate where go the inhibition arrow. *)
      (* For each events of this counterfactual causal core <that has no counterfactual-only cause|that as at least one factual cause>,
      find the last events in the factual trace among those that we blocked that prevent it (same method as above). Indicate in the counterfactual core the origin of these inhibition arrows. *)
      (* Update the factual core : compute a new factual causal core with all the previous added events + events with an inhibitive arrow <+ other factual events of the counterfactual core if we want to have more links with the factual core at the end>. *)
      aux factual counterfactuals events_in_story
    )
   in aux factual [] [eoi_id]

let compute_extended_story model trace rule_name config =
  (* We have to set IDs in the trace *)
  let trace = List.mapi (fun i s -> set_id (i+1) s) trace in
  (* Determining event of interest *)
  let eoi_id = get_first_rule_event model rule_name trace in
  (* Compute factual causal core (just the subtrace) *)
  let factual_subtrace = compute_causal_core model trace eoi_id in
  (* Add counterfactual parts *)
  let (factual_subtrace, counterfactual_parts) = add_counterfactual_parts model trace eoi_id config factual_subtrace in
  (* Merge the factual causal core and all the counterfactual causal cores. Depending on the details wanted by the user, we can :
      - Merge everything by merging together nodes that represent the same event
      - Keep only counterfactual-only events of counterfactual cores
      Don't forget to put all the inhibition arrows that had been found.
  *)
  ()
