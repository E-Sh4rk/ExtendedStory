open Ext_tools
open Compute

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

    log "Loading the trace file." ;
    let ch = open_in !file in
    let json = Yojson.Basic.from_channel ch in
    let () = close_in ch in
    let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
    let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
    log "Trace file loaded !" ;

    let config =
    {
      nb_samples   = 10;
      threshold    = 0.5;
      follow_causal_core = true ;
      max_cf_inhibition_arrows = 1;
      max_fc_inhibition_arrows = 1;
      more_relations_with_factual = false;
      show_entire_counterfactual_stories = false;
    } in
    compute_extended_story env steps !rule_of_interest config
  )

let () = main ()
