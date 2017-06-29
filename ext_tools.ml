
let log s = print_string s ; print_newline () ; flush stdout

let list_max lst = List.fold_left max (List.hd lst) lst

let list_min lst = List.fold_left min (List.hd lst) lst

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

let same_type x y = match x, y with
| n, m when n >= 0 && m < 0 -> false
| n, m when n < 0 && m >= 0 -> false
| n, m -> true

let rec get_event id trace = match id, trace with
| _, [] -> None
| _, s::trace when get_id s = id -> Some s
| n, s::trace when not (same_type n (get_id s)) -> get_event id trace
| n, s::trace when n >= 0 && get_id s > id -> None
| n, s::trace when n < 0 && get_id s < id -> None
| _, s::trace -> get_event id trace
