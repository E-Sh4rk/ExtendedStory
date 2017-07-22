open Ext_tools
open Ext_story
open Ext_story_printer

let file = ref ""
let max_stories = ref max_int
let output_prefix = ref ""
let rule_of_interest = ref ""
let verbose = ref false
let dot_format = ref false
let config = ref "regular"

let options = [
  ("-o", Arg.Set_string output_prefix,
   "prefix for the output files");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest");
  ("-c", Arg.Set_string config,
   "predefined configuration : shortest|regular|complete. Add prefix faster_ for a faster computation.");
  ("--max", Arg.Set_int max_stories,
   "maximal number of stories to generate");
  ("--verbose", Arg.Set verbose,
   "more detailed output");
  ("--dot", Arg.Set dot_format,
   "use dot format instead of json");
]
let description = ""

let regular_config =
{
  compression_algorithm = kaflow_compression;
  give_cumulated_core_to_heuristic = false;
  heuristic    = Heuristics.heuristic_1 Heuristics.Persistence;
  nb_samples   = 25;
  trace_scoring_heuristic = Heuristics.scoring_1;
  threshold    = 1.0;
  max_counterfactual_exps = 5;
  cf_inhibitions_finding_mode = Consider_only_predicted_core;
  cf_activation_paths_compression = Minimize;
  fc_inhibitions_finding_mode = Consider_only_predicted_core;
  fc_activation_paths_compression = Minimize;
  max_inhibitors_added_per_factual_events = 3;
  max_inhibitors_added_per_cf_events = 1;
  add_common_events_to_both_cores = true;
  compute_inhibition_arrows_for_every_events = false;
  adjust_inhibition_arrows_with_new_core_predictions = true;
}
let faster_regular_config =
{
  regular_config with
  nb_samples = 10;
  trace_scoring_heuristic = Heuristics.scoring_shorter;
  cf_inhibitions_finding_mode = Consider_entire_trace;
  fc_inhibitions_finding_mode = Consider_entire_trace;
  adjust_inhibition_arrows_with_new_core_predictions = false;
}
let shortest_config =
{
  regular_config with
  add_common_events_to_both_cores = false;
  compute_inhibition_arrows_for_every_events = false;
}
let faster_shortest_config =
{
  shortest_config with
  nb_samples = 10;
  trace_scoring_heuristic = Heuristics.scoring_shorter;
  cf_inhibitions_finding_mode = Consider_entire_trace;
  fc_inhibitions_finding_mode = Consider_entire_trace;
  adjust_inhibition_arrows_with_new_core_predictions = false;
}
let complete_config =
{
  regular_config with
  add_common_events_to_both_cores = true;
  compute_inhibition_arrows_for_every_events = true;
}
let faster_complete_config =
{
  complete_config with
  nb_samples = 10;
  trace_scoring_heuristic = Heuristics.scoring_shorter;
  cf_inhibitions_finding_mode = Consider_entire_trace;
  fc_inhibitions_finding_mode = Consider_entire_trace;
  adjust_inhibition_arrows_with_new_core_predictions = false;
}

let get_first_eoi_after te rule_name i =
  let model = Trace_explorer.model te in
  let rec aux i = match i with
  | i when i > Trace_explorer.last_step_id te -> raise Not_found
  | i when get_step_name model (Trace_explorer.step i te) "" = rule_name -> i
  | i -> aux (i+1) in
  aux (i+1)

let main () = Printexc.record_backtrace true ;
  let () =
    Arg.parse
      options
      (fun f -> if !file = "" then file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      description in
  if !file = "" then
    prerr_string "Please specify a trace file."
  else if !rule_of_interest = "" then
    prerr_string "Please specify a rule."
  else
  (
    if !output_prefix = "" then output_prefix := "story" ;
    let choosen_config = ref regular_config in
    begin match !config with
    | "regular" -> choosen_config := regular_config
    | "faster_regular" -> choosen_config := faster_regular_config
    | "shortest" -> choosen_config := shortest_config
    | "faster_shortest" -> choosen_config := faster_shortest_config
    | "complete" -> choosen_config := complete_config
    | "faster_complete" -> choosen_config := faster_complete_config
    | _ -> logs "/!\\ Unknown config. Regular config will be used."
    end ;

    logs "Loading the trace file..." ;
    let te = Trace_explorer.load_from_file !file in
    logs "Done." ;

    let eoi = ref (-1) in
    try
    (
      for _=1 to !max_stories do
        logs "\nSearching next event of interest..." ;
        eoi := get_first_eoi_after te !rule_of_interest (!eoi);
        let es = compute_extended_story te (!eoi) (!choosen_config) in

        let ext = if !dot_format then ".dot" else ".json" in
        let oc = open_out (!output_prefix^"_"^(string_of_int (!eoi))^ext) in
        let fmt = Format.formatter_of_out_channel oc in
        let dot_options = 
              if !verbose then Story_printer.def_options_detailed
              else Story_printer.def_options_simple in
        let json_options = 
              if !verbose then Ext_story_json.def_options_detailed
              else Ext_story_json.def_options_simple in 
        if !dot_format then print_extended_story es Hiding_factual_events dot_options fmt
        else Ext_story_json.print_json_of_extended_story es (!eoi) (!choosen_config).compression_algorithm json_options oc ;
        close_out oc
      done
    ) with Not_found -> logs "All events of interest processed !"
  )

let () = main ()
