open Nfa


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

let reachables_from_start (n : nfa) (start : int) : int list = 
  let ts = n.transactions in
  (List.map
    (fun x -> match x with | (_, _, out) -> out)
    (List.filter 
      (fun x -> match x with 
        | (i, None, _) -> if i = start then true else false
        | _ -> false )
      ts)) @ [start]

let rec digin (ts : transaction list) (ss : int list) : int list = 
  let can_move = List.filter
    (fun t -> match t with 
      | (in_state, Some _, out) -> false
      | (in_state, None, out) -> 
          if List.mem in_state ss then true else false)
    ts
  in 
  let new_out = List.map 
    (fun ts -> match ts with | (_, _, out) -> out) can_move in
  let new_ss = (list_union new_out ss) in
  if new_ss = ss
  then new_ss
  else digin ts new_ss

let step (n : nfa) (ss : int list) (c : char) : int list = 
  let ts = n.transactions in 
  let edge_is_c = List.filter
    (fun t -> match t with 
      | (_, None, _)-> false
      | (in_state, Some e, _) -> 
          if c = e && (List.mem in_state ss) then true else false)
    ts
  in
  let reachables = List.map
    (fun t -> match t with
    | (_, _, out) -> out) edge_is_c
  in
  digin n.transactions reachables



let match_re (re : string) (str : string) : bool = 
  let tokens = Ast.string_to_char_list str in
  let {
    states       = s;
    alphabets    = a;
    transactions = t;
    start_state  = st;
    final_states = fs;
  } as n = make_nfa re in 
  let rec helper input next_starts = 
    match input with
    | [] -> next_starts
    | hd :: tl ->
        let new_ss = step n next_starts hd in
        helper tl new_ss
  in
  let ans = helper tokens (reachables_from_start n st) in
  let has_pass = List.map 
    (fun x -> List.mem x fs) ans
  in 
  (*(print_endline ""; Nfa.nfa_to_string n);*)
  if List.mem true has_pass 
  then true 
  else false

(*let _ = *)
  (*print_endline (string_of_bool (match_re "a*bc" "aaac"));*)
  (*let n = make_nfa "a*bc" in*)
  (*let new_ss = step n (reachables_from_start n n.start_state) 'a' in*)
  (*List.iter (fun x -> print_string ((string_of_int x) ^ " ")) new_ss *)

type d_transaction = state * alphabet * state

type dfa = {
  d_states : state list ;
  d_alphabets : alphabet list;
  d_transactions : d_transaction list;
  d_start_state : state ;
  d_final_states : state list ;
}

let move (n : nfa) (ss : state list) (c : char) : state list = 
  let ts = n.transactions in 
  let edge_is_c = List.filter
    (fun t -> match t with 
      | (_, None, _)-> false
      | (in_state, Some e, _) -> 
          if c = e && (List.mem in_state ss) then true else false)
    ts
  in
  let reachables = List.map
    (fun t -> match t with
    | (_, _, out) -> out) edge_is_c
  in
  reachables


let rec e_closure (n : nfa) (ss : state list) : state list = 
  let ts = n.transactions in
  let can_move = List.filter
    (fun t -> match t with 
      | (in_state, Some _, out) -> false
      | (in_state, None, out) -> 
          if List.mem in_state ss then true else false)
    ts
  in 
  let new_out = List.map 
    (fun ts -> match ts with | (_, _, out) -> out) can_move in
  let new_ss = (list_union new_out ss) in
  if new_ss = ss
  then new_ss
  else e_closure n new_ss

type set_to_set_trans = (state list * alphabet * state list)
type state_set = state list

let rec helper (n : nfa) (work_list : state_set list) (q : state_set list) 
               (st : set_to_set_trans list) (d : (state_set * state) list) =
  match work_list with
  | [] -> (q, st, d)
  | states :: rest ->
    let all_trans = 
      List.map (fun c -> (states, c, e_closure n (move n states c))) n.alphabets
    in
    let tobe_added_trans = List.filter
      (fun trans -> match trans with
      | (_, _, []) -> false
      | (_, _, _) -> true)
      all_trans
    in
    let tobe_added = List.map
      (fun trans -> match trans with
      | (_, _, set) -> set)
      tobe_added_trans
    in
    match tobe_added with
    | [] -> (q, st, d)
    | _ ->
      let new_stateset = List.fold_left 
        (fun acc set -> if List.mem set work_list then acc else set :: acc)
        []
        tobe_added 
      in
      let d_to_set = List.map (fun x -> (x, Nfa.gen_state_num ())) new_stateset in
      helper n (new_stateset @ rest) (new_stateset @ q) (tobe_added_trans @ st) (d_to_set @ d)

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
  let start = e_closure n [n.start_state] in 
  let (d_states, state_trans, dict) = helper n [start] [start] [] [(start, Nfa.gen_state_num ())] in
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
  print_endline "";
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
  List.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.d_final_states 

let () = 
  let a = Ast.parse "a*(b|c)*d" in
  let n = make_nfa "a*(b|c)*d" in
  nfa_to_string n ;
  let d = nfa_to_dfa n in
  dfa_to_string d
  
