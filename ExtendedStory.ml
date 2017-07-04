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

    logs "Loading the trace file." ;
    let ch = open_in !file in
    let json = Yojson.Basic.from_channel ch in
    let () = close_in ch in
    let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
    let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
    logs "Trace file loaded !" ;

    let config =
    {
      nb_samples   = 10;
      threshold    = 0.5;
      precompute_cf_cores = true ;
      max_cf_inhibition_arrows = 1;
      max_fc_inhibition_arrows_per_inhibator = 1;
      add_all_factual_events_involved_to_factual_core = false;
    } in
    let es = compute_extended_story env steps !rule_of_interest config in
    print_extended_story es
  )

let () = main ()
