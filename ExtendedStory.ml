open Ext_tools
open Ext_story
open Ext_story_printer

let file = ref ""
let max_stories = ref max_int
let output_prefix = ref ""
let rule_of_interest = ref ""
let verbose = ref false

let options = [
  ("-o", Arg.Set_string output_prefix,
   "prefix for the output file");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest");
  ("--max", Arg.Set_int max_stories,
   "maximal number of stories to generate");
  ("--verbose", Arg.Set verbose,
   "print annotated dot files");
]
let description = ""

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

    logs "Loading the trace file..." ;
    let te = Trace_explorer.load_from_file !file in
    logs "Done." ;

    let config =
    {
      compression_algorithm = kaflow_compression;
      heuristic    = Heuristics.heuristic_1 Heuristics.Persistence;
      nb_samples   = 10;
      trace_rejection_heuristic = Heuristics.rejection_1;
      threshold    = 0.95;
      max_counterfactual_parts = 2 ;
      keep_rejected_cf_parts = false;
      precompute_cores = true ;
      max_cf_inhibition_arrows = 2;
      max_fc_inhibition_arrows_per_inhibator = 2;
      add_all_factual_events_involved_to_factual_core = false;
    } in
    let eoi = ref (-1) in
    try
    (
      for _=1 to !max_stories do
        logs "\nSearching next event of interest..." ;
        eoi := get_first_eoi_after te !rule_of_interest (!eoi);
        let es = compute_extended_story te (!eoi) config in

        let oc = open_out (!output_prefix^"_"^(string_of_int (!eoi))^".dot") in
        let fmt = Format.formatter_of_out_channel oc in
        let options = 
              if !verbose then Story_printer.def_options_detailed
              else Story_printer.def_options_simple in
        print_extended_story es Hiding_factual_events options fmt ;
        close_out oc
      done
    ) with Not_found -> logs "All events of interest processed !"
  )

let () = main ()
