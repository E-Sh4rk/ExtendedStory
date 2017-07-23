open Ext_tools
open Global_trace
open Format

type json_exportation_options =
  {
    show_activations : bool ;
    show_edges_infos : bool ;
    show_event_ids : bool ;
    remove_duplicate_edges : bool
  }

let def_options_detailed =
  {
    show_activations = true ;
    show_edges_infos = true ;
    show_event_ids = true ;
    remove_duplicate_edges = true
  }

let def_options_simple =
  {
    show_activations = false ;
    show_edges_infos = false ;
    show_event_ids = false ;
    remove_duplicate_edges = true
  }

let label_of_event trace options i =
  let env = get_model trace in
  let id = get_global_id trace i in
  let label = 
    begin
      match get_step trace i with
      | Trace.Rule (rule_id, ev, _) ->
        asprintf "%s" (rule_ast_name env rule_id)
      | Trace.Obs (obs_name, _, _) -> obs_name
      | Trace.Init actions ->
        let agents = Story_printer.introduced_agent_sorts actions in
        Story_printer.intro_name env agents
      | Trace.Pert (s, _, _) -> s
      | Trace.Subs _ | Trace.Dummy _ -> assert false
    end
  in
  if options.show_event_ids then sprintf "[%d] %s" id label
  else label

let main_event_to_node trace options clickable i =
  let id = get_global_id trace i in
  let label = label_of_event trace options i in
  let node_type = if IntSet.mem id clickable then "clickable" else "factual" in
  `Assoc [("id", `String (string_of_int id)) ; ("label", `String label) ; ("type", `String node_type)]

let precedence_to_edge _ _ (src,dest) =
  let source = string_of_int src and target = string_of_int dest in
  let edge_type = "precedence" in
  let label = "" in
  `Assoc [("label", `String label) ; ("source", `String source) ; ("target", `String target) ; ("type", `String edge_type)]

let causal_relation_to_edge trace options edge_type (src,c,dest) =
  let env = get_model trace in
  let source = string_of_int src and target = string_of_int dest in
  let label = if options.show_edges_infos then
  (
    let (Grid.Constr (x, _v)) = c in
    asprintf "%a" (Grid.print_var env) (Grid.Var x)
  )
  else "" in
  `Assoc [("label", `String label) ; ("source", `String source) ; ("target", `String target) ; ("type", `String edge_type)]

let activation_to_edge trace options (dest,c,src) =
  causal_relation_to_edge trace options "activation" (src,c,dest)

let inhibition_to_edge trace _ options (src,c,dest) =
  causal_relation_to_edge trace options "inhibition" (src,c,dest)

let compute_and_export_edges trace options =
  let precedence = Ext_story_printer.compute_precedence trace in
  let edges = List.map (precedence_to_edge trace options) precedence in

  if options.show_activations then
  (
    let activations = Ext_story_printer.compute_activation trace in
    let activations = List.filter (fun (_,c,_) -> Story_printer.important_constr c) activations in
    let edges_2 = List.map (activation_to_edge trace options) activations in
    edges@edges_2
  )
  else edges

let exp_event_to_node f_trace cf_trace options blocked factual i =
  let (trace, opp_trace) = if factual then (f_trace, cf_trace) else (cf_trace, f_trace) in
  let id = get_global_id trace i in
  let label = label_of_event trace options i in

  let common = search_global_id opp_trace id <> None in
  if common && not factual then None else
  (
    let node_type = if common then "common"
      else if IntSet.mem id blocked then "blocked"
      else if factual then "factual" else "counterfactual" in
    Some (`Assoc [("id", `String (string_of_int id)) ; ("label", `String label) ; ("type", `String node_type)])
  )

let experiment_to_json options (fact, cf, inh, blocked) =
  (* Bindings *)
  let bindings = `List (List.map (fun s -> `String (string_of_int s)) (IntSet.elements blocked)) in
  (* Nodes *)
  let nodes_1 = List.map (exp_event_to_node fact cf options blocked true) (n_first_integers (length fact)) in
  let nodes_2 = List.map (exp_event_to_node fact cf options blocked false) (n_first_integers (length cf)) in
  let nodes = nodes_1 @ nodes_2 in
  let nodes = List.filter (function None -> false | _ -> true) nodes in
  let nodes = List.map (function None -> assert false | Some n -> n) nodes in
  let nodes = `List nodes in
  (* Edges *)
  let edges_1 = compute_and_export_edges fact options in
  let edges_2 = compute_and_export_edges cf options in
  let edges_3 = List.map (inhibition_to_edge fact cf options) (InhSet.elements inh) in
  let edges = edges_1@edges_2@edges_3 in
  let edges = if options.remove_duplicate_edges then List.sort_uniq Pervasives.compare edges else edges in
  let edges = `List edges in

  `Assoc [("bindings",bindings) ; ("nodes",nodes) ; ("edges",edges)]

let print_json_of_extended_story (_,fact,_,exps) options oc =
  (* Export main graph *)
  let blocked = List.fold_left (fun acc (_,_,_,lst) -> IntSet.union acc lst) IntSet.empty exps in
  let nodes = List.map (main_event_to_node fact options blocked) (n_first_integers (length fact)) in
  let nodes = `List nodes in
  let edges = compute_and_export_edges fact options in
  let edges = if options.remove_duplicate_edges then List.sort_uniq Pervasives.compare edges else edges in
  let edges = `List edges in
  let main = `Assoc [("nodes", nodes) ; ("edges", edges)] in

  (* Export experiments *)
  let experiments = List.map (experiment_to_json options) exps in
  let experiments = `List experiments in

  let json = `Assoc [("main", main) ; ("experiments", experiments)] in
  Yojson.Basic.pretty_to_channel oc json ; flush oc

