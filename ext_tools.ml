open Grid

(* ----- Utils ----- *)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end
module IntSet = Set.Make(Int)

let logs s = print_string s ; print_newline () ; flush stdout

let dbg s = logs ("[DBG] "^s)

let min_c c e1 e2 = match c e1 e2 with
| -1 -> e1 | 0 -> e1 | 1 -> e2 | _ -> assert false
let max_c c = min_c (fun a b -> - (c a b))

let list_max_c c lst = List.fold_left (max_c c) (List.hd lst) lst
let list_min_c c lst = List.fold_left (min_c c) (List.hd lst) lst

let rec cut_after_index i lst = match i, lst with
  | _, [] -> []
  | 0, s::_ -> [s]
  | n, s::lst -> s::(cut_after_index (n-1) lst)

let n_first_integers n =
  let rec aux n acc = match n with
  | n when n < 0 -> acc
  | n -> aux (n-1) (n::acc)
  in aux (n-1) []

(* ----- Kappa ----- *)

module ASet = Set.Make(Agent)

let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

let get_step_name model step default = match step with
  | Trace.Rule (rule_id,_,_) -> rule_ast_name model rule_id
  | Trace.Obs (name,_,_) -> name
  | Trace.Pert (name,_,_) -> name
  | _ -> default

let agents_involved (constrs:constr list) =
  let aggregate_agent acc constr = match constr with
  | Constr (Internal_state (agent,_), _) -> ASet.add agent acc
  | Constr (Binding_state (agent,_), _) -> ASet.add agent acc
  | Constr (Agent_existence agent, _) -> ASet.add agent acc
  in List.fold_left aggregate_agent ASet.empty constrs

let agents_modified actions =
  let constrs = translate_actions actions
  in agents_involved constrs

let get_time_of_step ts default = match ts with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.Trace.Simulation_info.story_time
  | Trace.Init _ -> 0.0
  | _ -> default

let get_rule_id ts default = match ts with
  | Trace.Rule (rid,_,_) -> rid
  | _ -> default
