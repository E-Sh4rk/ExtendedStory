
(* ----- Utils ----- *)

let log s = print_string s ; print_newline () ; flush stdout

let min_c c e1 e2 = match c e1 e2 with
| -1 -> e1 | 0 -> e1 | 1 -> e2
let max_c c = min_c (fun a b -> - (c a b))

let list_max_c c lst = List.fold_left (max_c c) (List.hd lst) lst
let list_min_c c lst = List.fold_left (min_c c) (List.hd lst) lst

(* ----- Kappa ----- *)

let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

(* ----- Specific ----- *)

type step = Causal_core_shared.step_id * Trace.step

let set_id id step = match step with
  | (i, Trace.Rule (rid,inst,infos)) -> (i,Trace.Rule (rid,inst,{infos with story_id=id}))
  | (i, Trace.Pert (str,inst,infos)) -> (i,Trace.Pert (str,inst,{infos with story_id=id}))
  | (i, Trace.Obs (str,inst,infos)) -> (i,Trace.Obs (str,inst,{infos with story_id=id}))
  | (i, s) -> (i, s)

let set_index i (_,s) = (i,s)
let get_index (i,_) = i

let get_id_from_ts step = match step with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_id
  | _ -> raise Not_found

let get_id (i,s) = match s with
  | Trace.Rule _ | Trace.Pert _ | Trace.Obs _ -> get_id_from_ts s
  | Trace.Init _ -> i
  | _ -> failwith "Invalid trace !"

let get_time (i,step) default = match step with
  | Trace.Rule (_,_,infos) | Trace.Pert (_,_,infos) | Trace.Obs (_,_,infos)
  -> infos.story_time
  | Trace.Init _ -> 0.0
  | _ -> default

let rec nb_of_events_before_time trace time = match trace with
  | [] -> 0
  | s::trace when get_time s 0.0 >= time -> 0
  | s::trace -> 1 + (nb_of_events_before_time trace time)

let same_sign x y = match x, y with
| n, m when n >= 0 && m < 0 -> false
| n, m when n < 0 && m >= 0 -> false
| n, m -> true

let get_event id trace =
  let rec aux trace index = match id, trace with
  | _, [] -> None
  | _, s::trace when get_id s = id -> Some (index, s)
  | n, s::trace when not (same_sign n (get_id s)) -> aux trace (index+1)
  | n, s::trace when n >= 0 && get_id s > id -> None
  | n, s::trace when n < 0 && get_id s < id -> None
  | _, s::trace -> aux trace (index+1)
  in aux trace 0
