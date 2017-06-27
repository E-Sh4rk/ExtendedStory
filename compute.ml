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
  | s::trace -> get_first_rule_event model rule_name trace

let trace_succeed eoi_id trace = match Array.length trace with
  | 0 -> false
  | n -> begin match trace.(n-1) with
    | Resimulation.Factual_happened (Rule (rid,inst,infos)) when infos.story_id = eoi_id -> true
    | _ -> false
  end

let resimulate_and_sample nb eoi_id model block_pred stop_pred trace =
  let rec aux nb acc = match nb with
    | 0 -> acc
    | n -> let (nb_failed,wit) = acc in 
    let ctrace = Array.of_list (resimulate model block_pred stop_pred trace) in
    if trace_succeed eoi_id ctrace then
    aux (nb-1) (nb_failed,wit)
    else
    aux (nb-1) (nb_failed+1,Some ctrace)
  in aux nb (0,None)

let compute_causal_core model trace eoi_id =
  let set_id id step = match step with
  | Rule (rid,inst,infos) -> Rule (rid,inst,{infos with story_id=id+1})
  | Pert (str,inst,infos) -> Pert (str,inst,{infos with story_id=id+1})
  | Obs (str,inst,infos) -> Obs (str,inst,{infos with story_id=id+1})
  | _ -> step in
  let subtrace = [] in
  (* We have also to set IDs *)
  List.mapi set_id subtrace

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
      let Some ctrace = ctrace in
      (* Convert the counterfactual trace to a regular trace (don't forget to set IDs for counterfactual events) *)
      (* Find the last events that has inhibited the first event of the causal core that has been blocked :
      it is the last events that changed the value of a tested logical site from a good value to a wrong value. *)
      (* Select the first (earliest) of these events and compute its causal core. Add this counterfactual causal core to the list and indicate where go the inhibition arrow. *)
      (* For each events of this counterfactual causal core <that has no counterfactual-only cause|that as at least one factual cause>,
      find the last events in the factual trace among those that we blocked that prevent it (same method as above). Indicate in the counterfactual core the origin of these inhibition arrows. *)
      (* Update the factual core : compute a new factual causal core with all the previous added events + events with an inhibitive arrow <+ other factual events of the counterfactual core if we want to have more links with the factual core at the end>. *)
      aux factual counterfactuals events_in_story
    )
   in aux factual [] [eoi_id]

let compute_extended_story model trace rule_name config =
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
