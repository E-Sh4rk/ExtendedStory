
let log s = print_string s ; print_newline () ; flush stdout

let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

let set_id id step = match step with
  | Trace.Rule (rid,inst,infos) -> Trace.Rule (rid,inst,{infos with story_id=id})
  | Trace.Pert (str,inst,infos) -> Trace.Pert (str,inst,{infos with story_id=id})
  | Trace.Obs (str,inst,infos) -> Trace.Obs (str,inst,{infos with story_id=id})
  | _ -> step

let get_id step = match step with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_id
  | _ -> 0

  let rec get_event id trace = match trace with
  | [] -> None
  | s::trace when get_id s = id -> Some s
  | s::trace when get_id s > id -> None
  | s::trace -> get_event id trace
