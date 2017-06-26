(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
(*                                                                            *)
(*  Author / Maintainer: Jonathan Laurent (jonathan.laurent@cs.cmu.edu)       *)
(*                                                                            *)
(******************************************************************************)

(** Resimulation algorithm that serves as a basis for 
    counterfactual reasoning. *)


module Algos = struct

  (*  This function solves the following problem:
      Given `n` boxes containing white and black balls, pick one ball
      from each box conditionned to the fact that at least one ball has
      to be black.

      Input: [prob] is an array of size `n` whose ith element
      is the probability to draw a black ball from the ith box.Agent

      Returns: an array of `n` booleans, [true] representing a black ball
      and [false] a white ball. *)

  let draw_at_least_one random_state probs = 

      let draw_exp p =
          let x = Random.State.float random_state 1.0 in
          int_of_float (ceil (log1p (-. x) /. log1p (-. p))) in

      let array_min t = 
          Array.fold_left min t.(0) t in

      let r = Array.map draw_exp probs in
      let m = array_min r in
      Array.map (fun e -> e = m) r

  (* Alternate implementation *)
  let draw_at_least_one' probs =
      let draw p = (Random.float 1.0) <= p in
      let f p1 lst = p1 /. (1. -. (List.fold_left (fun acc e -> acc*.(1. -. e)) 1. (p1::lst))) in
      let rec _draw_at_least_one probs = 
      match probs with
      | [] -> []
      | hd::tl when draw (f hd tl) -> true::(List.map draw tl)
      | _hd::tl -> false::(_draw_at_least_one tl)
      in Array.of_list (_draw_at_least_one (Array.to_list probs))

end

module Util = struct

  let array_zip_with f t t' =
    let n = min (Array.length t) (Array.length t') in
    Array.init n (fun i -> f t.(i) t'.(i))

  let array_product = Array.fold_left ( * ) 1

  let partition_int_collection f col = 
    let n = IntCollection.size col in
    let true_c = IntCollection.create n in
    let false_c = IntCollection.create n in
    IntCollection.fold (fun i () ->
      if f i then IntCollection.add i true_c
      else IntCollection.add i false_c
    ) col () ;
    (true_c, false_c)

  let int_collection_singleton i = 
    let c = IntCollection.create 1 in
    IntCollection.add i c ;
    c

  exception Found

  let inefficient_int_collection_mem i c = 
    try
      IntCollection.fold (fun j () -> 
        if i = j then raise Found
      ) c () ; false
    with Found -> true

  let rec range a b = if a > b then [] else a :: range (a+1) b

  let rec map_filter f = function
    | [] -> []
    | x::xs -> 
      match f x with
      | Some y -> y :: map_filter f xs
      | None -> map_filter f xs

end

open Util


(******************************************************************************)
(* DIVERGENT INSTANCES                                                        *)
(******************************************************************************)

type div_instances_message = 
    | New_cur_mixture of Edges.t
    (** This message is sent when the current mixture is updated.
        A reference to the current mixture is needed by `is_divergent` *)

    | New_reference_state of Edges.t * (Pattern.id * Agent.t) list
    (** This message is sent when the reference graph is updated. 
        It contains:
         + The new reference graph
         + A list of observables whose status (convergent or divergent) 
           might have changed *)
    | Flush_roots_buffer
    (** Calls to [Divergent_instances.update_roots] are put in a cache
        and executed when this message is received *)


module Divergent_instances 
: Instances_sig.S with type message = div_instances_message =
struct

  type mod_ccs_cache = (int, unit) Hashtbl.t

  type t = {
    conv_roots : Roots.t ;
    div_roots  : Roots.t ;

    reference_mixture : Edges.t ;
    current_mixture : Edges.t ;
    (* Has to be updated at each event loop using `send_message` *)

    precomputed_unary_patterns : Pattern.Set.t ;
    (* Useful to call `update_roots` in  *)
    
    domain : Pattern.Env.t ;
    (* The domain has to be stored in [t] because of 
       the `is_divergent` function. *)

    roots_buffer : (bool * mod_ccs_cache * Pattern.id * int) Queue.t
    (* Calls to [Divergent_instances.update_roots] are put in this cache
       and executed when the message [Flush_roots_buffer] is received *)
  }

  let break_apart_cc insts graph cache ccs =
    let conv_roots = Roots.break_apart_cc insts.conv_roots graph cache ccs in
    let div_roots  = Roots.break_apart_cc insts.div_roots  graph cache ccs in
    { insts with conv_roots ; div_roots }

  let merge_cc insts cache ccs = 
    let conv_roots = Roots.merge_cc insts.conv_roots cache ccs in
    let div_roots  = Roots.merge_cc insts.div_roots cache ccs in
    { insts with conv_roots ; div_roots }

  let empty env =
    { reference_mixture = Edges.empty ~with_connected_components:true ; 
      current_mixture = Edges.empty ~with_connected_components:true ;
      domain = Model.domain env ;
      precomputed_unary_patterns = Model.unary_patterns env ;
      conv_roots = Roots.empty env ;
      div_roots = Roots.empty env ;
      roots_buffer = Queue.create () }

  let debug_print fmt insts = 
    Format.fprintf fmt "[CONVERGENT]:@;%a@.@.[DIVERGENT]:%a@."
      Roots.debug_print insts.conv_roots
      Roots.debug_print insts.div_roots


  exception Divergent

  (* [root] must be the root of an embedding of [pat_id] in [graph]  *)
  let is_divergent domain ref_graph graph pat_id root =
    let nav = Pattern.Env.to_navigation domain pat_id in
    let ag = (root, Edges.get_sort root graph) in
    match Navigation.concretize ag graph nav with
    | None -> false
    (* Means the observable is outdated and so it will 
        be removed soon, so we can give any answer *)
    | Some nav ->

    let compare_step ((ag, s), arr) =
      match arr with
      | Navigation.ToNode (ag', s') ->
        begin
          assert (Edges.link_exists ag s ag' s' graph) ;
          if not (Edges.link_exists ag s ag' s' ref_graph) then raise Divergent
        end
      | Navigation.ToNothing ->
        begin
          assert (Edges.is_free ag s graph) ;
          if not (Edges.is_free ag s ref_graph) then raise Divergent
        end
      | Navigation.ToInternal st ->
        begin
          assert (Edges.is_internal st ag s graph) ;
          if not (Edges.is_internal st ag s ref_graph) then raise Divergent
        end in

    try begin List.iter compare_step nav ; (false) end
    with Divergent -> (true)

  let update_roots insts is_positive_update _unary_pats _graph cache id root =
    Queue.push (is_positive_update, cache, id, root) insts.roots_buffer

  let update_roots_now insts is_positive_update unary_pats graph cache id root = 
    let update is = 
      Roots.update_roots is is_positive_update unary_pats graph cache id root in
    if not is_positive_update then 
    begin
      update insts.div_roots ;
      update insts.conv_roots
    end
    else if is_divergent insts.domain insts.reference_mixture graph id root then
      update insts.div_roots
    else
      update insts.conv_roots
  

  (* Update the status of an observable (convergent or divergent) *)
  let update_obs_status insts unary_pats graph pat_id root =
    let dummy_cache = Hashtbl.create 10 in
    update_roots_now insts false unary_pats graph dummy_cache pat_id root ;
    update_roots_now insts true unary_pats graph dummy_cache pat_id root

 
  let incorporate_extra_pattern st pat_id roots =
    let is_divergent = is_divergent st.domain 
      st.reference_mixture st.current_mixture pat_id in
    let div_roots, conv_roots = 
      Util.partition_int_collection is_divergent roots in
    Roots.incorporate_extra_pattern st.div_roots pat_id div_roots ;
    Roots.incorporate_extra_pattern st.conv_roots pat_id conv_roots
  

  type message = div_instances_message

  let send_message msg st =
      match msg with
      | New_cur_mixture edges -> { st with current_mixture = edges }
      | New_reference_state (edges, obs) ->
        begin
          let st = { st with reference_mixture = edges } in
          let ups = st.precomputed_unary_patterns in
          let graph = st.current_mixture in
          obs |> List.iter (fun (pat_id, (root, _root_ty)) -> 
            update_obs_status st ups graph pat_id root
          ) ; st
        end
      | Flush_roots_buffer ->
        begin
          Queue.iter (fun (is_pos, cache, pat_id, root) ->
            update_roots_now st is_pos st.precomputed_unary_patterns 
              st.current_mixture cache pat_id root
          ) st.roots_buffer ;
          Queue.clear st.roots_buffer ;
          st
        end

  let get_unary_maps st (pat1, pat2) =
    let map1_conv = Roots.of_unary_pattern pat1 st.conv_roots in
    let map2_conv = Roots.of_unary_pattern pat2 st.conv_roots in
    let map1_div  = Roots.of_unary_pattern pat1 st.div_roots  in
    let map2_div  = Roots.of_unary_pattern pat2 st.div_roots  in
    map1_conv, map2_conv, map1_div, map2_div


  (** {6 Counting instances } *)

  let number_of_instances insts pats =
    let cc_counts xs = 
      Array.map (fun pat -> Roots.number xs pat) pats in
    let convs = cc_counts insts.conv_roots in
    let divs = cc_counts insts.div_roots in
    array_product (array_zip_with (+) convs divs) - array_product convs


  let number_of_unary_instances_in_cc st (pat1, pat2) = 
    let map1_conv, map2_conv, map1_div, map2_div = 
        get_unary_maps st (pat1, pat2) in
    fun cc -> 
      let size_cc m = Mods.IntSet.size
        (Mods.IntMap.find_default Mods.IntSet.empty cc m) in
      let n1_conv = size_cc map1_conv in
      let n2_conv = size_cc map2_conv in
      let n1_div  = size_cc map1_div  in
      let n2_div  = size_cc map2_div  in
      (n1_conv + n1_div) * (n2_conv + n2_div) - n1_conv * n2_conv


  (** {6 Picking instances } *)

  module type INT_SET = sig
    type t
    val size : t -> int
    val random : Random.State.t -> t -> int option
    val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
  end

  let pick (type set) (module S : INT_SET with type t = set) 
    random_state (conv_sets : set array) (div_sets : set array) =

    let n  = Array.length conv_sets in
    let n' = Array.length div_sets  in
    assert (n = n') ;
    let probs = Array.init n (fun i ->
      let nc = S.size conv_sets.(i) in
      let nd = S.size div_sets.(i) in
      float_of_int nd /. float_of_int (nc + nd)
    ) in
    let is_div = Algos.draw_at_least_one random_state probs in
    Array.init n (fun i ->
      if is_div.(i)
      then S.random random_state div_sets.(i)
      else S.random random_state conv_sets.(i)
    )
    |> Array.map (Option_util.unsome (-1))


  let pick_unary_instance_in_cc st random_state (pat1, pat2) =
    let map1_conv, map2_conv, map1_div, map2_div = 
      get_unary_maps st (pat1, pat2) in
    fun cc ->
      let rts m =
        (Mods.IntMap.find_default Mods.IntSet.empty cc m) in
      let roots = pick (module Mods.IntSet) random_state 
        [| rts map1_conv ; rts map2_conv |] 
        [| rts map1_div ; rts map2_div |] in
      roots.(0), roots.(1)

  
  let fold_picked_instance st random_state pats ~init f =
    let conv_sets = 
      Array.map (fun pat_id -> Roots.of_pattern pat_id st.conv_roots) pats in
    let div_sets = 
      Array.map (fun pat_id -> Roots.of_pattern pat_id st.div_roots) pats in
    let roots = pick (module IntCollection) random_state conv_sets div_sets in
    
    let rec aux i acc = 
      if i >= Array.length pats then acc else
      match acc with
        | None -> None
        | Some acc ->
          begin
            let root = roots.(i) in
            assert (root <> (-1)) ;
            let acc = f i pats.(i) root acc in
            aux (i+1) acc
          end
    in aux 0 (Some init)


  (** {6 Enumerating instances } *)

  let fold_combinations (type set) (module S : INT_SET with type t = set)
    conv_sets div_sets ~init f =

    let n  = Array.length conv_sets in
    let n' = Array.length div_sets  in
    assert (n = n') ;

    let size = 
        array_product (array_zip_with (+) 
          (Array.map S.size conv_sets) (Array.map S.size div_sets))
        - array_product (Array.map S.size conv_sets) in

    let closed_list = Hashtbl.create size in

    let call l acc = 
      if Hashtbl.mem closed_list l then acc
      else begin
        let acc = f l acc in
        Hashtbl.add closed_list l () ;
        acc
      end in

    let take_div_at_i i acc =
      let tab = Array.make n (-1) in
      let rec aux j acc = 
        let take_root r acc = begin tab.(j) <- r  ; aux (j+1) acc end in
        if j >= n then call (Array.to_list tab) acc
        else
          let acc = S.fold take_root div_sets.(j) acc in
          if (j = i) then acc
          else S.fold take_root conv_sets.(j) acc
      in aux 0 acc in

    let acc = List.fold_right take_div_at_i (Util.range 0 (n-1)) init in
    assert (Hashtbl.length closed_list = size) ;
    acc


  module List_as_set : INT_SET with type t = int list = struct
    type t = int list
    let size = List.length
    let random st l = Some (List_util.random st l)
    let fold = List.fold_right
  end

  let _test_fold_combinations () = 
    let conv_sets = [| [11; 12]; [11]; [11; 12] |] in
    let div_sets = [| [21]; [21]; [21; 22] |] in
    fold_combinations (module List_as_set) conv_sets div_sets ~init:()
      (fun l () ->
        l |> List.iter (fun i ->
          Printf.printf "%d " i
        ) ;
        print_newline ()
      )


  let process_excp pats set = function
  | None -> ()
  | Some (pat, root) ->
      pats
      |> Array.to_list
      |> List.mapi (fun i x -> (i, x))
      |> List.filter (fun (_, pat') -> Pattern.is_equal_canonicals pat pat')
      |> List.map fst
      |> List.iter (fun i ->
        (* [i] must be sent ro [root] *)
        if inefficient_int_collection_mem root set.(i)
        then set.(i) <- int_collection_singleton root
        else set.(i) <- IntCollection.create 0
      )

  
  let fold_instances ?excp st pats ~init f =
    let conv_sets = 
      Array.map (fun pat_id -> Roots.of_pattern pat_id st.conv_roots) pats in
    let div_sets = 
      Array.map (fun pat_id -> Roots.of_pattern pat_id st.div_roots) pats in
    process_excp pats conv_sets excp ;
    process_excp pats div_sets excp ;
    fold_combinations (module IntCollection) conv_sets div_sets ~init f


  let map_fold4 ~def map1 map2 map3 map4 ~init f =
    let keys m = List.map fst (Mods.IntMap.bindings m) in
    let all_keys = List.sort_uniq compare 
      (keys map1 @ keys map2 @ keys map3 @ keys map4) in
    List.fold_left (fun acc key ->
      let v1 = Mods.IntMap.find_default def key map1 in
      let v2 = Mods.IntMap.find_default def key map2 in
      let v3 = Mods.IntMap.find_default def key map3 in
      let v4 = Mods.IntMap.find_default def key map4 in
      f key v1 v2 v3 v4 acc
    ) init all_keys


  let fold_unary_instances st (pat1, pat2) ~init f =
     let map1_conv, map2_conv, map1_div, map2_div = get_unary_maps st (pat1, pat2) in
     map_fold4 ~def:Mods.IntSet.empty map1_conv map2_conv map1_div map2_div ~init 
      (fun _ rts1_conv rts2_conv rts1_div rts2_div acc ->
        fold_combinations (module Mods.IntSet) 
          [| rts1_conv ; rts2_conv |] [| rts1_div ; rts2_div |] ~init:acc (fun l acc ->
          match l with
          | [r1; r2] -> f (r1, r2) acc
          | _ -> assert false
        )
      )

end


(******************************************************************************)
(* RESIMULATION TYPES AND HELPERS                                             *)
(******************************************************************************)

module Modified_rule_interpreter = 
  Generic_rule_interpreter.Make(Divergent_instances)

module Mri = Modified_rule_interpreter

type state = {
  graph : Modified_rule_interpreter.t ;
  ref_state : Replay.state ;
  counter : Counter.t ;
  model : Model.t ;
}

type step =
  | Factual_happened of Trace.step
  | Factual_did_not_happen of bool * Trace.step
  | Counterfactual_happened of Trace.step

let debug_print_resimulation_step env f = 
  let open Format in
  let pp_step = Trace.print_step ~compact:true ~env in
  function
    | Factual_happened step -> 
      fprintf f "[F] %a" pp_step step
    | Factual_did_not_happen (_blocked, step) ->
      fprintf f "[X] %a" pp_step step
    | Counterfactual_happened step -> fprintf f "[C] %a" pp_step step


(** {6 Compute observables to update after a factual step } *)

let valid_positive_transformation_on_same_sites edges = 
  let open Primitives.Transformation in  
  let handle_link (ag, s) =
    if not (Edges.is_agent ag edges) then [] else
    match Edges.link_destination (Agent.id ag) s edges with
    | None -> [Freed (ag, s)]
    | Some (ag', s') -> [Linked ((ag, s), (ag', s'))] in

  function
  | Agent ag -> if Edges.is_agent ag edges then [Agent ag] else []
  | Freed q -> handle_link q
  | Linked (q, q') -> handle_link q @ handle_link q'
  | NegativeWhatEver _ | NegativeInternalized _ -> assert false
  | PositiveInternalized (ag, s, _) ->
    if Edges.is_agent ag edges then
      let st = Edges.get_internal (Agent.id ag) s edges in
      [PositiveInternalized (ag, s, st)]
    else []

let valid_positive_transformations_on_same_sites edges trans =
  List.concat (List.map (valid_positive_transformation_on_same_sites edges) trans)

let actions_of_step = function
  | Trace.Subs _ -> ([],[])
  | Trace.Rule (_,e,_) | Trace.Pert (_,e,_) ->
    (e.Instantiation.actions,e.Instantiation.side_effects_dst)
  | Trace.Init y -> (y,[])
  | Trace.Obs (_,_,_) -> ([],[])
  | Trace.Dummy _ -> ([],[])

let observables_to_update state step =
  let actions, side_effects_dst = actions_of_step step in
  let trans = Primitives.Transformation.positive_transformations_of_actions
    (Model.signatures state.model) side_effects_dst actions in
  let mod_trans = valid_positive_transformations_on_same_sites (Mri.get_edges state.graph) trans in
  let obs, deps = Mri.obs_from_transformations (Model.domain state.model) (Mri.get_edges state.graph) mod_trans in
  obs, deps


(** Small helper functions *)

let time st = Counter.current_time st.counter

let update_outdated_activities st =
  let graph, _ =
      Mri.update_outdated_activities (fun _ _ _ -> ())
        st.model st.counter st.graph in
  { st with graph }

let debug_print_obs_updates model (obs, deps) =
  let pp_obs_list fmt l = 
    l |> List.iter (fun (pat, (root, _)) ->
      let domain = Model.domain model in
      Format.fprintf fmt "({@[<h>%a@]}, %d)@." 
        (Pattern.print ~new_syntax:false ~domain ~with_id:true) pat root)
  in
  Format.printf "@.%a@.@.%d@." pp_obs_list obs (Operator.DepSet.size deps)

let step_time step = 
  match Trace.simulation_info_of_step step with
  | None -> assert false
  | Some info -> info.Trace.Simulation_info.story_time



(******************************************************************************)
(* RESIMULATION                                                               *)
(******************************************************************************)

let init model random_state =
  let counter = Counter.create ~plot_period:(Counter.DT 1.0) 
    ?max_event:None ?max_time:None ?init_t:None ?init_e:None in
  { counter ;
    graph = Mri.empty ~with_trace:true random_state model counter ;
    ref_state = Replay.init_state ~with_connected_components:true ;
    model }



let do_factual_step blocked step st =
  let ref_state, _ = 
      Replay.do_step (Model.signatures st.model) st.ref_state step in
  let time' = ref_state.Replay.time in
  let st = { st with ref_state } in
  Counter.one_time_advance st.counter (time' -. time st) ;
  let obs, deps = observables_to_update st step in
  (* debug_print_obs_updates st.model (obs, deps) *)
  let graph = Mri.add_outdated_dependencies deps st.graph in
  let graph = Mri.send_instances_message
    (New_reference_state (ref_state.Replay.graph, obs)) graph in
  let st = { st with graph ; ref_state } in

  if Replay.is_step_triggerable_on_edges (Mri.get_edges st.graph) step
     && not blocked
  then begin
    let graph = Mri.update_edges_from_actions ~outputs:(fun _ -> ())
      (Model.signatures st.model) st.counter (Model.domain st.model)
      st.graph (actions_of_step step) in
    let graph = Mri.send_instances_message 
        (New_cur_mixture (Mri.get_edges graph)) graph in
    let graph = Mri.send_instances_message Flush_roots_buffer graph in
    Some (Factual_happened step) ,
    update_outdated_activities { st with graph }
  end else begin
    Some (Factual_did_not_happen (blocked, step)) ,
    update_outdated_activities st
  end



let do_counterfactual_step dt st =
  let gen_step = ref None in
  let receive_step = function
    | Data.TraceStep step -> gen_step := Some (Counterfactual_happened step)
    | _ -> () in
  let applied_rule, _, graph =
    Mri.apply_rule ~outputs:receive_step ~maxConsecutiveClash:3 
      st.model st.counter st.graph in
  let graph = Mri.send_instances_message 
    (New_cur_mixture (Mri.get_edges graph)) graph in
  let graph = Mri.send_instances_message Flush_roots_buffer graph in
  let st = { st with graph } in
  if applied_rule = None then !gen_step, st (* null_event *) 
  else begin
    Counter.one_time_advance st.counter dt ;
    !gen_step, update_outdated_activities st
  end



type next_step_kind =
  | Next_is_factual
  | Next_is_counterfactual

exception End_of_resimulation

let do_step next_ref_step_opt st =

  let get_next_ref_step, next_ref_step_time, block_next_ref_step =
    match next_ref_step_opt with
    | None -> (fun () -> assert false), infinity, false
    | Some (s, t, b) -> (fun () -> s), t, b in

  let div_activity = Mri.activity st.graph in
  let rd = Random.State.float (Mri.get_random_state st.graph) 1.0 in
  let dt = abs_float (log rd /. div_activity) in
  let next_div_step_time = time st +. dt in
 
  let time', next_step_kind =
    if next_ref_step_time <= next_div_step_time
    then next_ref_step_time, Next_is_factual
    else next_div_step_time, Next_is_counterfactual in
  
  if time' = infinity then raise End_of_resimulation
  else
    match next_step_kind with
    | Next_is_counterfactual ->
      let step, st = do_counterfactual_step dt st in
      false, step, st
    | Next_is_factual -> 
      let step, st = do_factual_step block_next_ref_step (get_next_ref_step ()) st in
      true, step, st



let rec loop_until_consummed ~stop emit next_ref_step_opt st =
  let consummed, opt_step, st = do_step next_ref_step_opt st in
  begin match opt_step with
  | None -> ()
  | Some step -> (emit step ; if stop step then raise End_of_resimulation)
  end ;
  if consummed then st
  else loop_until_consummed ~stop emit next_ref_step_opt st


let resimulate ?stop_after:(stop=fun _ -> false) ~blocked ~rcv_step trace_file =
  let state model = init model (Random.get_state ()) in
  let emit model rs = rcv_step model rs in
  try
    trace_file |> Trace.fold_trace_file (fun model st step ->
      let next = (step, step_time step, blocked model step) in
      loop_until_consummed ~stop (emit model) (Some next) st
    ) state
    |> ignore
  with End_of_resimulation -> ()