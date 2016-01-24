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

let get_splits states c : Trans_in_states_set.t = 
  failwith "unimplemented"

(*val splits : dfa -> state_set -> rest_worklist -> c -> trans_between_set*)
let split d set rest_worklist c : Trans_in_states_set.t = 
  let module S = States_set in
  let module T = Trans_in_states_set in
  let trans = d.d_transactions in
  S.fold rest_worklist ~init:T.empty 
    ~f:(fun acc states -> T.union (get_splits states c) acc)
  




(*val hopcroft : work_list -> partition set -> d_state -> trans -> *)
               (*dict -> (d_state * d_trans * dict)*)
let rec hopcroft d work_list cur_partition trans dict : (States_set.t * Trans_in_states_set.t * state Dict.t) =
  let module S = States_set in
  let module T = Trans_in_states_set in
  match S.choose work_list with
  | None -> (cur_partition, trans, dict)
  | Some set ->
      let rest_worklist = S.remove work_list set in
      let split_on_chars = List.filter_map
        (Alphabet_set.elements d.d_alphabets)
        ~f:(fun c -> 
          let splits = split d set rest_worklist c in
          match T.is_empty splits with
          | true -> None
          | false -> Some (T.elements splits))
        |> List.fold_left ~init:[]
            ~f:(fun acc x -> match x with
                | [] -> acc  
                | l -> l @ acc)
      in
      let new_states = 
        List.map split_on_chars
          ~f:(fun (s_in, c, s_out) -> s_out)
      in 
      let new_states_set = S.of_list new_states in
      let new_worklist = S.union new_states_set rest_worklist in
      let new_partition = S.union new_states_set cur_partition in
      let new_trans = T.union trans (T.of_list split_on_chars) in
      let new_dict = List.fold_left ~init:dict new_states
            ~f:(fun acc x -> Dict.add acc ~key:x ~data:(Nfa.gen_state_num ()))
      in
      hopcroft d new_worklist new_partition new_trans new_dict

let minimize (d : dfa) = 
  let module S = States_set in
  let module T = Trans_in_states_set in
  let partition = S.add (S.singleton (State_set.singleton d.d_start_state)) d.d_final_states in
  let work_list = partition in
  let dict = List.fold_left 
    [State_set.singleton d.d_start_state; d.d_final_states] 
    ~init:Dict.empty
    ~f:(fun acc x -> Dict.add acc ~key:x ~data:(Nfa.gen_state_num ()))
  in
  hopcroft d work_list partition T.empty dict






(*let () = *)
  (*let n = make_nfa "a(b|c)*" in*)
  (*let d = nfa_to_dfa n in*)
  (*dfa_to_string d*)

  
