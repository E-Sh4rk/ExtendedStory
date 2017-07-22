
type json_exportation_options =
  {
    show_activations : bool ;
    show_edges_infos : bool
  }

let def_options_detailed =
  {
    show_activations = true ;
    show_edges_infos = true
  }

let def_options_simple =
  {
    show_activations = false ;
    show_edges_infos = false
  }

let print_json_of_extended_story (fact,exps) options oc =
  (* Compute and export main graph *)

  let main = `Null in
  (* Compute and export experiments *)
  let json = `Assoc [("main", main) ; ("experiments", `List [])] in
  Yojson.Basic.pretty_to_channel oc json

