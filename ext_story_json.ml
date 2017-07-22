open Ext_tools
open Global_trace
open Format

type json_exportation_options =
  {
    show_activations : bool ;
    show_edges_infos : bool ;
    show_event_ids : bool
  }

let def_options_detailed =
  {
    show_activations = true ;
    show_edges_infos = true ;
    show_event_ids = true
  }

let def_options_simple =
  {
    show_activations = false ;
    show_edges_infos = false ;
    show_event_ids = false
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

let activation_to_edge trace options (dest,c,src) =
  let env = get_model trace in
  let source = string_of_int src and target = string_of_int dest in
  let edge_type = "activation" in
  let label = if options.show_edges_infos then
  (
    let (Grid.Constr (x, _v)) = c in
    asprintf "%a" (Grid.print_var env) (Grid.Var x)
  )
  else "" in
  `Assoc [("label", `String label) ; ("source", `String source) ; ("target", `String target) ; ("type", `String edge_type)]

let print_json_of_extended_story (fact,exps) compression_algorithm options oc =
  (* Compute and export main graph *)
  let eoi_index = (length fact)-1 in
  let blocked_events = List.fold_left (fun acc (_,_,_,lst) -> IntSet.union acc lst) IntSet.empty exps in
  let blocked_events_index = IntSet.map (fun id -> match search_global_id fact id with None -> assert false | Some i -> i) blocked_events in
  let main_core = compression_algorithm (get_trace_explorer fact) (get_var_infos fact) (eoi_index::(IntSet.elements blocked_events_index)) in
  let main_subtrace = subtrace_of fact main_core in

  let nodes_lst = List.map (main_event_to_node fact options blocked_events) main_core in
  
  let precedence = Ext_story_printer.compute_precedence main_subtrace in
  let edges_lst = List.map (precedence_to_edge fact options) precedence in

  let edges_lst = if options.show_activations then
  (
    let activations = Ext_story_printer.compute_activation main_subtrace in
    let activations = List.filter (fun (_,c,_) -> Story_printer.important_constr c) activations in
    let edges_lst_2 = List.map (activation_to_edge fact options) activations in
    edges_lst@edges_lst_2
  )
  else edges_lst in

  let nodes = `List nodes_lst and edges = `List edges_lst in
  let main = `Assoc [("nodes", nodes) ; ("edges", edges)] in

  (* TODO :  Compute and export experiments *)


  let json = `Assoc [("main", main) ; ("experiments", `List [])] in
  Yojson.Basic.pretty_to_channel oc json ; flush oc

