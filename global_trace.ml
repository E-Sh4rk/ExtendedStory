open Ext_tools

(* Abstract types *)

type global_info = (int*int) array (* Associate an index with (id,order) *)
type trace_info = Trace_explorer.t * Causal_core.var_info_table
type t = trace_info * global_info

type global_info_builder = ( (int*int) list ) * int * int (* (list,next_rt_index,next_order) *)
type t_builder = (Trace.step list) * global_info_builder

(* Not exported *)

let set_global_id (_,gi) index id =
  let (_,order) = gi.(index) in
  gi.(index) <- (id,order)

let set_order (_,gi) index order =
  let (id,_) = gi.(index) in
  gi.(index) <- (id,order)

let nto_binary_search arr cmp value =
  let rec nearest_element i min max dir =
    match i+dir with
    | i when i > max || i < min -> None
    | i when cmp value arr.(i) = None -> nearest_element i min max dir
    | i when cmp value arr.(i) = Some dir || cmp value arr.(i) = Some 0 -> Some i
    | _ -> None
  in
  let rec aux i1 i2 = match i1, i2 with
  | i1, i2 when i1 > i2 -> None
  | i1, i2 -> begin match (i1+i2)/2 with
    | i when cmp arr.(i) value = None ->
    let ni = nearest_element i i1 i2 (-1) in
    let ni = if ni = None then nearest_element i i1 i2 1 else ni in
    begin match ni with
      | None -> None
      | Some ni when ni > i -> aux ni i2
      | Some ni -> aux i1 ni
    end
    | i when cmp arr.(i) value = Some 0 -> Some i
    | i when cmp arr.(i) value = Some (-1) -> aux (i+1) i2
    | i when cmp arr.(i) value = Some 1 -> aux i1 (i-1)
    | _ -> assert false
  end in
  aux 0 ((Array.length arr)-1)

let unidir_binary_search arr first pred =
  let rec aux i1 i2 = match i1, i2 with
    | i1, i2 when i1 > i2 -> None
    | _, i2 when first && not (pred arr.(i2)) -> None
    | i1, _ when not first && not (pred arr.(i1)) -> None
    | i1, _ when first && pred arr.(i1) -> Some i1
    | _, i2 when not first && pred arr.(i2) -> Some i2
    | i1, i2 -> let i = if first then (i1+i2)/2 else (i1+i2+1)/2 in
    begin match i with
      | i when pred arr.(i) -> if first then aux i1 i else aux i i2
      | i -> if first then aux (i+1) i2 else aux i1 (i-1)
    end
  in
  aux 0 ((Array.length arr)-1)

(* Exported *)

(* Basic functions *)
let get_global_id (_,gi) index =
  let (id,_) = gi.(index) in
  id

let get_order (_,gi) index =
  if index < 0 then (-1)
  else if index >= Array.length gi then max_int
  else (
    let (_,order) = gi.(index) in
    order
  )

let get_step ((te,_),_) index = Trace_explorer.step index te

let length (_,gi) = Array.length gi

let get_trace_explorer ((te,_),_) =  te

let get_model tr = Trace_explorer.model (get_trace_explorer tr)

let next_f_id = ref 0
let new_reference_trace te =
  Trace_explorer.Grid.build te ;
  let vi = Causal_core.init_var_infos te in
  let gi = Array.make ((Trace_explorer.last_step_id te) + 1) (0,0) in
  let gtr = ((te,vi),gi) in
  for i=0 to (Array.length gi) - 1 do
    set_global_id gtr i (!next_f_id) ;
    set_order gtr i i ;
    next_f_id := !next_f_id + 1
  done ; gtr

let copy ((te,vi),gi) = ((te,vi),Array.copy gi)

let subtrace_of tr core =
  let core = List.sort_uniq Pervasives.compare core in
  let subtrace = List.map (get_step tr) core in
  let nte = Trace_explorer.of_trace (get_model tr) subtrace in
  Trace_explorer.Grid.build nte ;
  let nvi = Causal_core.init_var_infos nte in
  let core = Array.of_list core in
  let ngi = Array.make (Array.length core) (0,0) in
  let ntr = ((nte,nvi),ngi) in
  for i=0 to (Array.length ngi) - 1 do
    set_global_id ntr i (get_global_id tr core.(i)) ;
    set_order ntr i (get_order tr core.(i))
  done ; ntr

let new_counterfactual_trace_builder () = ([],([],0,0))

let last_cf_id = ref 0
let add_counterfactual_step rtr (csteps,(cgi,ri,o)) cs =
  match cs with
  | Resimulation.Counterfactual_happened s ->
  last_cf_id := !last_cf_id - 1 ;
  (s::csteps,((!last_cf_id,o)::cgi,ri,o+1))
  | Resimulation.Factual_did_not_happen (_,s) ->
  assert (s = get_step rtr ri) ;
  set_order rtr ri o ; (csteps,(cgi,ri+1,o+1))
  | Resimulation.Factual_happened s ->
  assert (s = get_step rtr ri) ;
  set_order rtr ri o ;
  (s::csteps,((get_global_id rtr ri,o)::cgi,ri+1,o+1))

let finalize_counterfactual_trace rtr (ctr,(cgi,ri,o)) =
  for i=ri to (length rtr) - 1 do
    set_order rtr i (o+i-ri)
  done ;
  let te = Trace_explorer.of_trace (get_model rtr) (List.rev ctr) in
  Trace_explorer.Grid.build te ;
  let vi = Causal_core.init_var_infos te in
  ((te,vi), Array.of_list (List.rev cgi))

(* Search functions : search ID, search order... *)
let search_global_id (_,gi) id =
  let cmp (id1,_) (id2,_) = match id1, id2 with
  | id1, id2 when id1 >= 0 && id2 < 0 -> None
  | id1, id2 when id2 >= 0 && id1 < 0 -> None
  | id1, id2 when id1 >= 0 && id2 >= 0 -> Some (Pervasives.compare id1 id2)
  | id1, id2 -> Some (- (Pervasives.compare id1 id2))
in nto_binary_search gi cmp (id,0)

let search_first_after_order (_,gi) order =
  let pred (_,o) = o > order in
  unidir_binary_search gi true pred

let search_last_before_order (_,gi) order =
  let pred (_,o) = o < order in
  unidir_binary_search gi false pred

(* Trace analysis functions *)
let get_tests tr index = Trace_explorer.Grid.tests index (get_trace_explorer tr)

let get_actions tr index = Trace_explorer.Grid.actions index (get_trace_explorer tr)

let get_var_infos ((_,vi),_) = vi

let get_history ((_,vi),_) var = Causal_core.get_modifications_history var vi

(* Misc *)
let reset_ids () = last_cf_id := 0 ; next_f_id := 0

(* Printing functions *)
let print_core tr fmt core =
  List.iter (fun i -> Format.fprintf fmt "%d ; " (get_global_id tr i)) core
let print fmt tr =
  let core = n_first_integers (length tr) in
  List.iter (fun i -> Format.fprintf fmt "%d ; " (get_global_id tr i)) core
let print_full fmt tr =
  let core = n_first_integers (length tr) in
  List.iter (fun i -> Format.fprintf fmt "%d (%d,%d) : %s\n" i (get_global_id tr i) (get_order tr i)
  (get_step_name (get_model tr) (get_step tr i) "")) core ;
  Format.fprintf fmt "------------------"