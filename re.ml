open Nfa
open Myset

exception IllegalInput of string


let make_nfa (str : string) : nfa = 
  let tree = Ast.parse str in
  match tree with
  | Some regex -> Nfa.regex_to_nfa regex
  | None -> raise (IllegalInput "input string cannot be parsed")

let rec list_union l1 l2 = 
  match l1 with
  | [] -> l2
  | hd :: tl ->
      if List.mem hd l2 
      then list_union tl l2
      else list_union tl (hd :: l2)

let list_diff l1 l2 = 
  List.fold_right
    (fun x acc ->
      if List.mem x l2
      then acc
      else x :: acc)
    l1 []

let move (n : nfa) (cur_reachable : state_set) (c : char) : state_set = 
  let ts = n.transactions in 
  let edge_is_c = Transaction_set.(filter
    (fun t -> match t with 
      | (_, None, _)-> false
      | (in_state, Some e, _) -> 
          if c = e && (State_set.mem in_state cur_reachable) then true else false)
    ts)
  in
  let reachables = List.map
    (fun t -> match t with
    | (_, _, out) -> out) (Transaction_set.elements edge_is_c)
  in
  State_set.of_list reachables

let rec e_closure (n : nfa) (cur_reachable : state_set) : state_set  = 
  let ts = n.transactions in
  let e_moveable = Transaction_set.filter
    (fun t -> match t with 
      | (in_state, Some _, out) -> false
      | (in_state, None, out) -> 
          if State_set.mem in_state cur_reachable then true else false)
    ts
  in 
  let e_moveable_list = Transaction_set.elements e_moveable in
  let e_reachable_states = State_set.of_list 
  (List.map
    (fun ts -> match ts with | (_, _, out) -> out) e_moveable_list) in
  let new_reachable = (State_set.union e_reachable_states cur_reachable ) in
  if new_reachable= cur_reachable
  then new_reachable
  else e_closure n new_reachable

type set_to_set_trans = (state list * alphabet * state list)
type state_set = state list

let rec subset_construct (n : nfa) (work_list : states_set) (q : states_set) 
               (d_trans : Trans_in_states_set.t) (dict : state Dict.t) =
  match States_set.is_empty work_list with
  | true -> (q, d_trans, dict)
  | false ->
      let module T = Trans_in_states_set in
      let module S = State_set in
      let selected = States_set.choose work_list in
      let trans_on_all_chars = T.of_list
        (List.map (fun c -> (selected, c, move n (e_closure n selected) c)) 
                  (Alphabet_set.elements n.alphabets))
      in
      let new_states_groups = 
        T.filter (fun trans -> match trans with
                  | (_, _, out) -> S.is_empty out)
                 trans_on_all_chars
      in
      match T.is_empty new_states_groups with
      | true -> subset_construct n 

        


let int_enumerate l h = 
 let rec helper l h col = 
    if l > h then col else helper l (h - 1) (h :: col)
  in helper l h [] 

let get_val dict set = 
  List.assoc set dict

let transform_trans (trans : set_to_set_trans list) (dict : (state_set * state) list) = 
  List.map
    (fun t -> match t with
      | (in_set, c, out_set) -> (get_val dict in_set, c ,get_val dict out_set))
    trans

let state_intersect l1 l2 = 
  List.filter 
    (fun x -> if List.mem x l2 then true else false)
    l1

let get_finals (dict : (state_set * Nfa.state) list) (finals : Nfa.state list) : state list = 
  let set_with_finals = 
    List.filter
      (fun d -> match d with 
      | (states, d) -> match state_intersect states finals with
        | [] -> false
        | _ -> true)
      dict
  in List.map 
      (fun x -> match x with 
      | (_, d) -> d)
      set_with_finals

let get_start dict start = 
  let set_with_start = 
    List.filter
      (fun d -> match d with
      | (states, d) -> if List.mem start states then true else false)
      dict
  in 
  let d_start = List.map (fun x -> match x with | (_, d) -> d) set_with_start in
  match d_start with
  | d :: [] -> d
  | _ -> failwith "more than one start state"

let get_states dict = 
  List.map (fun x -> match x with | (_, d) -> d) dict


let nfa_to_dfa (n : nfa) : dfa = 
  let start = e_closure n (State_set.singleton n.start_state) in 
  let work_list = States_set.singleton start
  let q = work_list in
  let dict = Dict.singleton start Nfa.gen_state_num () in
  let d_trans_set = Transaction_set.empty in
  let (d_states, state_trans, dict) = subset_construct n work_list q d_trans_set dict in
  let new_states = get_states dict in
  let new_trans = transform_trans state_trans dict in
  let new_finals = get_finals dict n.final_states in
  {
    d_states = new_states ;
    d_alphabets = n.alphabets;
    d_transactions = new_trans;
    d_start_state = get_start dict n.start_state;
    d_final_states = new_finals;  
  }



let dfa_to_string (res : dfa) : unit =
  print_endline "DFA : ";
  print_string "states = ";
  List.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.d_states ;
  print_endline "";
  print_string "alphabets = ";
  List.iter (fun a -> print_string (Char.escaped a)) res.d_alphabets ;
  print_endline "";
  List.iter (fun (a, b, c) ->
    print_string ((string_of_int a) ^ " -> " ^ (Char.escaped b) ^ " -> " ^ (string_of_int c) ^ " " ))
  res.d_transactions;
  print_endline "";
  print_endline (string_of_int res.d_start_state);
  print_string "final_states = ";
  List.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.d_final_states ;
  print_endline ""


let minimize (d : dfa) : dfa = 
  let partition = [(list_diff d.states d.final_states); d.final_states] in
  let work_list = partition in
  hopcroft work_list partition [] [] []



let hopcroft 


(*let () = *)
  (*let n = make_nfa "a*b*d" in*)
  (*nfa_to_string n ;*)
  (*let d = nfa_to_dfa n in*)
  (*dfa_to_string d*)
  
