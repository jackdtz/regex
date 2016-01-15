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
   

let states_move (s : state list) (a : alphabet option) (ts : transaction list) : state list = 
  failwith ""

let move (s : state) (a : alphaber) (ts : transaction list) : state = 
  failwith ""

let follow_epsilon (s : state) (ts : transaction list) : state list = 
  state_move [s] None ts

let states_follow_epsilon (s : state list) (ts : transaction list) : state list = 
  state_move s None ts


let helper (n : nfa) (work_list : state list list) (d_states : state list list) 
           (d : dict) (ts : transaction list) : d_states * d * t = 
  match work_list with
  | [] -> (d_states, d, t)
  | hd :: tl ->
    List.map
      (fun c -> 
        let t = states_follow_epsilon (states_move hd c ts) ts in
        if (List.mem t d_state)
        then (d_states, d, t)
        else
          helper n (t :: tl) (t :: d_states) ((Nfa.gen_state_num (), t) :: d) ts






let nfa_to_dfa (n : nfa) : dfa = 
  let d0_states = state_follow_epsilon n.start_state n_ts in
  helper n d0_state [d0_states] ((Nfa.gen_state_num(), do_states) :: empty_dict) nfa.transactions

