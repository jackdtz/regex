open Core.Std
open Nfa
open Datatypes

exception IllegalInput of string
exception IllegalTransformation of string
exception MultipleStartStates of string

let string_of_state_set = Nfa.string_of_state_set

let debug_flag = false

let print_trans trans = 
  print_endline "trans: ";
  Trans_in_states_set.iter
    ~f:(fun (s_in, c, s_out) ->
      Nfa.string_of_state_set s_in;
      print_string " -> ";
      print_string (Char.escaped c);
      print_string " -> ";
      Nfa.string_of_state_set s_out;
      print_endline "")
  trans

let debug_info debug_flag cur_set work_list q d_trans = 
  match debug_flag with
  | false -> print_string ""
  | true  -> 
      print_endline "" ;
      print_string "Current work set: " ;
      Nfa.string_of_state_set cur_set ;
      print_endline "";
      print_trans d_trans ;
      print_endline ""; 
      print_endline "new_worklist : " ;
      States_set.iter ~f:(fun x ->  Nfa.string_of_state_set x; print_endline "") work_list;
      print_endline "" ;
      print_endline "new_q : " ;
      States_set.iter ~f:(fun x ->  Nfa.string_of_state_set x; print_endline "") q;
      print_endline ""

let make_nfa str = 
  let tree = Ast.parse str in
    match tree with
    | Some regex -> Nfa.regex_to_nfa regex
    | None -> raise (IllegalInput "input string cannot be parsed")


let move ts cur_reachable c = 
  let edge_is_c = Transaction_set.(filter
    ~f:(fun t -> match t with 
      | (_, None, _)-> false
      | (in_state, Some e, _) -> 
          if c = e && (State_set.mem cur_reachable in_state) then true else false)
    ts)
  in
  let reachables = List.map
    ~f:(fun t -> match t with
    | (_, _, out) -> out) (Transaction_set.elements edge_is_c)
  in
  State_set.of_list reachables

let rec e_closure n cur_reachable = 
  let ts = n.transactions in
  let e_moveable = Transaction_set.filter
    ~f:(fun t -> match t with 
      | (in_state, Some _, out) -> false
      | (in_state, None, out) -> 
          if State_set.mem cur_reachable in_state then true else false)
    ts
  in 
  let e_moveable_list = Transaction_set.elements e_moveable in
  let e_reachable_states = State_set.of_list 
  (List.map
    ~f:(fun ts -> match ts with | (_, _, out) -> out) e_moveable_list) in
  let new_reachable = (State_set.union e_reachable_states cur_reachable ) in
  match State_set.compare new_reachable cur_reachable with
  | 0 -> new_reachable
  | _ -> e_closure n new_reachable

let rec subset_construct n work_list q d_trans dict =
  match States_set.choose work_list with
  | None -> (q, d_trans, dict)
  | Some set ->
      debug_info debug_flag set work_list q d_trans ;
      let module T = Trans_in_states_set in
      let module S = States_set in
      let rest_worklist = S.remove work_list set in
      let trans_on_all_chars = List.filter_map
         ~f:(fun c -> let states = e_closure n (move n.transactions set c) in
                match State_set.is_empty states with
                | true -> None 
                | false -> Some (set, c, states) )
         (Alphabet_set.elements n.alphabets)
      in
      let new_state_groups = List.map ~f:(fun (_, _, out) -> out) trans_on_all_chars in
      match new_state_groups with
      | [] -> subset_construct n rest_worklist q d_trans dict
      | _ ->
          let new_states_set = S.filter
            ~f:(fun set -> not (S.mem q set)) (S.of_list new_state_groups) in
          let new_worklist = S.union new_states_set rest_worklist in
          let new_q = S.union q new_states_set in
          let new_trans = T.union d_trans (T.of_list trans_on_all_chars) in
          let mapping_dict = List.fold_left
            ~init:dict
            new_state_groups
            ~f:(fun acc group -> Dict.add acc group (Nfa.gen_state_num ()))
          in 
          subset_construct n new_worklist new_q new_trans mapping_dict

let get_states dict = 
  Dict.data dict |> State_set.of_list

let transform_trans trans_in_set dict =
  let module T = Trans_in_states_set in
  let module D = D_Transaction_set in
  T.elements trans_in_set
  |> List.map ~f:(fun (s_in, c, s_out) ->
      match Dict.find dict s_in, Dict.find dict s_out with
      | Some d1, Some d2 -> (d1, c, d2)
      | _ -> raise (IllegalTransformation ""))
  |> D.of_list

let get_finals finals dict = 
  let not_empty set = not (State_set.is_empty set) in
  Dict.filter dict
    ~f:(fun ~key:set ~data:_ -> State_set.(inter set finals |> not_empty))
  |> Dict.data
  |> State_set.of_list

let get_start start dict = 
  let l = 
    Dict.filter dict
      ~f:(fun ~key:set ~data:_ -> State_set.mem set start)
    |> Dict.data
  in match l with
  | hd :: [] -> hd
  | _ -> raise (MultipleStartStates "")
  
let rec nfa_to_dfa n = 
  let start = e_closure n (State_set.singleton n.start_state) in 
  let work_list = States_set.singleton start in
  let q = work_list in
  let dict = Dict.singleton start (Nfa.gen_state_num ()) in
  let d_trans_set = Trans_in_states_set.empty in
  let (d_states, state_trans, dict) = subset_construct n work_list q d_trans_set dict in
  let new_states = get_states dict in
  let new_trans = transform_trans state_trans dict in
  let new_finals = get_finals n.final_states dict in
  {
    d_states = new_states ;
    d_alphabets = n.alphabets;
    d_transactions = new_trans;
    d_start_state = get_start n.start_state dict;
    d_final_states = new_finals;  
  }

let dfa_to_string res =
  print_endline "DFA : ";
  print_string "states = ";
  State_set.iter ~f:(fun a -> print_string ((string_of_int a) ^ " ")) res.d_states ;
  print_endline "";
  print_string "alphabets = ";
  Alphabet_set.iter ~f:(fun a -> print_string (Char.escaped a)) res.d_alphabets ;
  print_endline "";
  D_Transaction_set.iter ~f:(fun (a, b, c) ->
    print_string ((string_of_int a) ^ " -> " ^ (Char.escaped b) ^ " -> " ^ (string_of_int c) ^ " " ))
  res.d_transactions;
  print_endline "";
  print_endline (string_of_int res.d_start_state);
  print_string "final_states = ";
  State_set.iter ~f:(fun a -> print_string ((string_of_int a) ^ " ")) res.d_final_states ;
  print_endline ""
  

let next_state_on_c state trans c = 
  let module S = States_set in
  let module T = Transaction_set in
  T.find_map trans
    ~f:(fun (s_in, ch, s_out) -> match s_in = state, ch = c with
          | true, true -> Some s_out
          | _ -> None)


let get_image d set c : State_set.t = 
  let module S = State_set in
  let module D = D_Transaction_set in
  let trans = d.d_transactions in
  D.fold trans
    ~init:State_set.empty
    ~f:(fun acc (s_in, ch, s_out) -> 
        match S.mem set s_out, c = ch with
        | true, true -> S.add acc s_out
        | _, _ -> acc)


let inter_with_partitions partition set = 
  let module S = States_set in
  S.elements partition |>
  List.map ~f:(fun par_set -> State_set.inter set par_set)

let diff_of_iter_partition partition inters = 
  let module S = States_set in
  List.map2_exn
    (S.elements partition)
    inters
    ~f:(fun part inter -> State_set.diff part inter)

(* 
 *  DFA Minimization Algorithm 
 *  https://www.clear.rice.edu/comp412/Lectures/L07Lex-4.pdf
 *  for getting new partitions
 *)
let iterate inters diffs partition work_list = 
  let module S = State_set in
  let module SS = States_set in
  let inter_diff_set = 
  List.map2_exn 
    inters
    diffs
    ~f:(fun inter diff -> (inter, diff))
  in
  List.fold2_exn
    inter_diff_set 
    (States_set.elements partition)
    ~init:(partition, work_list)
    ~f:(fun (p, w) (inter, diff) cur_par ->             (*p is partition and w is work list*)
      match S.is_empty inter, S.is_empty diff with
      | _, true | true, _ -> (p, w)
      | false, false ->
          let rest_partition = States_set.remove p cur_par in
          let new_partition = SS.add (SS.add rest_partition inter) diff in
          match States_set.mem w cur_par with
          | true -> let rest_worklist = States_set.remove w cur_par in
              let new_worklist = SS.add (SS.add rest_worklist inter) diff in
              (new_partition, new_worklist)
          | false ->
              match S.compare inter diff with
              | 1 -> 
                  let new_worklist = States_set.add w diff in
              (new_partition, new_worklist)
              | _ ->
                  let new_worklist = States_set.add w inter in
              (new_partition, new_worklist))






(*val hopcroft : work_list -> partition set -> d_state -> trans -> *)
               (*dict -> (d_state * d_trans * dict)*)
let rec hopcroft partition work_list =
  let module S = States_set in
  let module T = Trans_in_states_set in
  let module A = Alphabet_set in
  match S.choose work_list with
  | None -> partition
  | Some set ->
      print_endline "";
      print_endline "Selected set: " ;
      Nfa.string_of_state_set set ;
      print_endline "";
      print_endline "partitions: " ; 
      (S.iter partition ~f:(fun set ->(Nfa.string_of_state_set set))) ;
      print_endline "" ;
      print_endline "inters: ";
      let inter_with_set = inter_with_partitions partition set in
      (List.iter inter_with_set ~f:(fun set ->(Nfa.string_of_state_set set))) ;
      print_endline "" ;
      let diff_inter_par = diff_of_iter_partition partition inter_with_set in
      print_endline "diff: ";
      (List.iter diff_inter_par ~f:(fun set ->(Nfa.string_of_state_set set))) ;
      print_endline "" ;
      let (new_partition, new_worklist) = 
        iterate inter_with_set diff_inter_par partition work_list 
      in
      print_endline "new partitions: ";
      (S.iter new_partition ~f:(fun set ->(Nfa.string_of_state_set set))) ;
      print_endline "" ;
      print_endline "new work_list: ";
      (S.iter new_worklist ~f:(fun set ->(Nfa.string_of_state_set set))) ;
      print_endline "" ;
      hopcroft new_partition new_worklist

let get_new_partitions (d : dfa) = 
  let module S = States_set in
  let module T = Trans_in_states_set in
  let all_except_finals = (State_set.diff d.d_states d.d_final_states) in
  let work_list = S.(add (singleton all_except_finals) d.d_final_states)  in
  let partition = work_list in
  hopcroft partition work_list


let find_dtrans trans states = 
  let module D = D_Transaction_set in
  let one_of_state = match State_set.choose states with
    | None -> failwith "get empty partition"
    | Some s -> s
  in
  D.fold
    trans
    ~init:D.empty
    ~f:(fun acc (s_in, c, s_out) ->
      match s_in = one_of_state with
      | false -> acc
      | true  -> D.add acc (s_in, c, s_out))


let set_contains_state state partitions = 
  match 
    States_set.find partitions 
    ~f:(fun partition -> State_set.mem partition state)
  with
  | None -> failwith "set not found"
  | Some set -> set

let get_sets_trans partition trans_on_partition all_partition = 
    D_Transaction_set.elements trans_on_partition 
  |> List.map ~f:(fun (_, c, s_out ) -> (partition, c, set_contains_state s_out all_partition))
  |> Trans_in_states_set.of_list

let replace_set_with_state set_trans dict all_partitions = 
  let module D = D_Transaction_set in
  let module S = States_set in
  let module T = Trans_in_states_set in
  let replace dict states = 
    match (Dict.find dict states) with
    | None -> failwith "states set not found in dict"
    | Some d -> d
  in
  S.fold all_partitions
  ~init:D.empty
  ~f:(fun acc partition ->
    let start_is_par = T.filter set_trans 
      ~f:(fun (set_in, c, set_out) -> 
        match State_set.compare set_in partition with
        | 0 -> true | _ -> false)
    in
    let new_trans = T.fold start_is_par
    ~init:D.empty
    ~f:(fun acc (s_in, c, s_out) ->
        D.add acc ((replace dict s_in), c, (replace dict s_out)))
    in D.union acc new_trans)



let minimize d = 
  let module S = States_set in
  let module T = Trans_in_states_set in
  let new_partitions = get_new_partitions d in
  let trans = d.d_transactions in
  let start = d.d_start_state in
  let finals = d.d_final_states in
  let m_dict = S.fold
    new_partitions
    ~init:Dict.empty
    ~f:(fun acc p -> Dict.add acc ~key:p ~data:(Nfa.gen_state_num ()))
  in
  let set_trans = 
    S.fold 
    new_partitions
    ~init:T.empty
    ~f:(fun acc partition -> 
      let trans_on_partition = find_dtrans trans partition in
      T.union acc
        (get_sets_trans partition trans_on_partition new_partitions))
  in
  let new_d_trans = replace_set_with_state set_trans m_dict new_partitions in
  let new_start = get_start start m_dict in 
  let new_states = get_states m_dict in
  let new_finals = get_finals finals m_dict in
  {
    d_states = new_states ;
    d_alphabets = d.d_alphabets ;
    d_transactions = new_d_trans ;
    d_start_state = new_start ;
    d_final_states = new_finals ;
  }


    
let () = 
  let n = make_nfa "(a|b)*abb" in
  let d = minimize (nfa_to_dfa n) in
  dfa_to_string d

  
