open Core.Std
open Nfa
open Datatypes

exception IllegalInput of string
exception IllegalTransformation of string
exception MultipleStartStates of string

let string_of_state_set = Nfa.string_of_state_set

let make_nfa (str : string) : nfa = 
  let tree = Ast.parse str in
    match tree with
    | Some regex -> Nfa.regex_to_nfa regex
    | None -> raise (IllegalInput "input string cannot be parsed")


let print_trans (trans : Trans_in_states_set.t) = 
  print_endline "trans: ";
  Trans_in_states_set.iter
    ~f:(fun (s_in, c, s_out) ->
      Nfa.string_of_state_set s_in;
      print_string " -> ";
      print_string (Char.escaped c);
      print_string " -> ";
      Nfa.string_of_state_set s_out)
  trans

let move (n : nfa) (cur_reachable : state_set) (c : char) : state_set = 
  let ts = n.transactions in
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

let rec e_closure (n : nfa) (cur_reachable : state_set) : state_set  = 
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

type set_to_set_trans = (state list * alphabet * state list)
type state_set = state list

let rec subset_construct (n : nfa) (work_list : states_set) (q : states_set) 
               (d_trans : Trans_in_states_set.t) (dict : state Dict.t) =
  match States_set.choose work_list with
  | None -> (q, d_trans, dict)
  | Some set ->
      print_string "Current work set: " ;
      Nfa.string_of_state_set set ;
      let module T = Trans_in_states_set in
      let module S = States_set in
      let rest_worklist = S.remove work_list set in
      let trans_on_all_chars = List.filter_map
         ~f:(fun c -> let states = e_closure n (move n set c) in
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
          print_trans new_trans ;
          print_string "new_states_set : " ;
          States_set.iter ~f:(fun x ->  Nfa.string_of_state_set x) new_states_set ;
          print_string "new_worklist : " ;
          States_set.iter ~f:(fun x ->  Nfa.string_of_state_set x) new_worklist;
          print_string "new_q : " ;
          States_set.iter ~f:(fun x ->  Nfa.string_of_state_set x) new_q;
          print_endline "";
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
  

let rec nfa_to_dfa (n : nfa) : dfa = 
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


let dfa_to_string (res : dfa) : unit =
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


(*let minimize (d : dfa) : dfa = *)
  (*let partition = [(list_diff d.states d.final_states); d.final_states] in*)
  (*let work_list = partition in*)
  (*hopcroft work_list partition [] [] []*)



(*let hopcroft *)


let () = 
  let n = make_nfa "a(b|c)*" in
  let d = nfa_to_dfa n in
  dfa_to_string d

  
