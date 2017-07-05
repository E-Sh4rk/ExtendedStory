open Trace.Simulation_info

(* ----- Utils ----- *)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end
module IntSet = Set.Make(Int)

let logs s = print_string s ; print_newline () ; flush stdout

let min_c c e1 e2 = match c e1 e2 with
| -1 -> e1 | 0 -> e1 | 1 -> e2 | _ -> assert false
let max_c c = min_c (fun a b -> - (c a b))

let list_max_c c lst = List.fold_left (max_c c) (List.hd lst) lst
let list_min_c c lst = List.fold_left (min_c c) (List.hd lst) lst

let rec cut_after_index i lst = match i, lst with
  | _, [] -> []
  | 0, s::_ -> [s]
  | n, s::lst -> s::(cut_after_index (n-1) lst)

let n_first_intergers n =
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

let get_name model (i,step) default = match step with
  | Trace.Rule (rule_id,inst,infos) -> rule_ast_name model rule_id
  | Trace.Obs (name,inst,infos) -> name
  | Trace.Pert (name,inst,infos) -> name
  | _ -> default

let agents_tested tests =
  let aggregate_agent acc test = match test with
  | Instantiation.Is_Here a -> ASet.add a acc
  | _ -> acc
  in List.fold_left aggregate_agent ASet.empty (List.flatten tests)

let agents_tested_ts ts = match ts with
  | Trace.Rule (_,inst,_) | Trace.Pert (_,inst,_)
  -> agents_tested inst.Instantiation.tests
  | Trace.Obs (_,tests,_) -> agents_tested tests
  | Trace.Init _ -> ASet.empty
  | _ -> ASet.empty

let get_time_ts ts default = match ts with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_time
  | Trace.Init _ -> 0.0
  | _ -> default
