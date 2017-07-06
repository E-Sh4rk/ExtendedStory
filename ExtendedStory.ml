open Ext_tools
open Ext_story
open Ext_story_printer

let file = ref ""
let output_prefix = ref ""
let rule_of_interest = ref ""

let options = [
  ("-o", Arg.Set_string output_prefix,
   "prefix for the output file");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest")
]
let description = ""

let get_first_eoi_after te rule_name i =
  let model = Trace_explorer.model te in
  let rec aux i = match i with
  | i when i > Trace_explorer.last_step_id te -> raise Not_found
  | i when get_step_name model (Trace_explorer.step i te) "" = rule_name -> i
  | i -> aux (i+1) in
  aux i

let main () =
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
      nb_samples   = 10;
      threshold    = 0.5;
      max_counterfactual_parts = 2 ;
      precompute_cf_cores = true ;
      max_cf_inhibition_arrows = 1;
      max_fc_inhibition_arrows_per_inhibator = 1;
      add_all_factual_events_involved_to_factual_core = false;
    } in
    try
    (
      logs "Searching first event of interest !" ;
      let eoi = get_first_eoi_after te !rule_of_interest 0 in
      let es = compute_extended_story te eoi config in

      let options =
      { ranksep            = 1.0 ;
        show_strong_deps   = true ;
        strong_deps_labels = true ;
        dump_grid          = true ;
        show_event_ids     = true ;
        font               = "CMU Serif" ;
      } in

      let oc = open_out (!output_prefix^".dot") in
      let fmt = Format.formatter_of_out_channel oc in
      print_extended_story es Hiding_factual_events options fmt ;
      close_out oc
    ) with Not_found -> logs "No event of interest found !"
  )

let () = main ()
