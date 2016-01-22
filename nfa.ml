open Core.Std
open Ast
open Datatypes

let state_num = ref (- 1) 

let gen_state_num () : state = 
  state_num := !state_num + 1;
  !state_num

let states_union = State_set.union 
let alphabets_union = Alphabet_set.union 
let trans_union = Transaction_set.union

let make_nfa states alps trans start finals = 
  {
    states = states ;
    alphabets = alps ;
    transactions = trans ;
    start_state = start ;
    final_states = finals ;
  }

let states_to_state states state edge  = 
  State_set.fold
    (fun x acc -> Transaction_set.add (x, edge, state) acc)
    states
    Transaction_set.empty

let rec regex_to_nfa (re : regex) : nfa = 
  match re with
  | Char c -> handle_char c
  | Concatenation (e1, e2) -> handle_concat e1 e2
  | Alternation (e1, e2) -> handle_altern e1 e2 
  | Closure e -> handle_closure e

and handle_char (c : char) : nfa = 
  let s1 = gen_state_num () in 
  let s2 = gen_state_num () in
  {
    states = State_set.(empty |> add s1);
    alphabets = Alphabet_set.singleton c ;
    transactions = Transaction_set.singleton (s1, Some c, s2);
    start_state = s1;
    final_states = State_set.(empty |> add s2);
  }

and handle_concat (e1 : regex) (e2 : regex) : nfa = 
  let nfa1, nfa2 = regex_to_nfa e1, regex_to_nfa e2 in
  let states1, states2 = nfa1.states, nfa2.states in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let final1, final2 = nfa1.final_states, nfa2.final_states in
  {
    states = states_union states1 states2 ;
    alphabets = alphabets_union alps1 alps2;
    transactions = trans_union (trans_union trans1 trans2) 
                               (states_to_state final1 nfa2.start_state None) ;
    start_state = nfa1.start_state ;
    final_states = nfa2.final_states ;
  }

and handle_altern (e1 : regex) (e2 : regex) = 
  let nfa1, nfa2 = regex_to_nfa e1, regex_to_nfa e2 in
  let states1, states2 = nfa1.states, nfa2.states in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let start1, start2 = nfa1.start_state, nfa2.start_state in
  let final1, final2 = nfa1.final_states, nfa2.final_states in
  let s1 = gen_state_num () in
  let s2 = gen_state_num () in
  {
    states = State_set.(states_union states1 states2 |> add s1 |> add s2) ;
    alphabets = alphabets_union alps1 alps2 ;
    transactions = Transaction_set.
        (union trans1 trans2 |> union (states_to_state final1 s2 None) 
                             |> union (states_to_state final2 s2 None)
                             |> add (s1, None, start1)
                             |> add (s1, None, start2));
    start_state = s1;
    final_states = State_set.(empty |> add s2);
  }

and handle_closure (e : regex) : nfa = 
  let s1 = gen_state_num () in 
  let s2 = gen_state_num () in
  let n = regex_to_nfa e in
  let finals_to_start = 
    states_to_state n.final_states n.start_state None
  in
  let finals_to_s2 = 
    states_to_state n.final_states s2 None
  in
  {
    states = State_set.(n.states |> add s1 |> add s2) ;
    alphabets = n.alphabets ;
    transactions = Transaction_set.
      (union n.transactions finals_to_s2 
        |> union finals_to_start
        |> add (s1, None, s2)
        |> add (s1, None, n.start_state)) ;
    start_state = s1 ;
    final_states = State_set.(empty |> add s2);
  }

let nfa_to_string res =
  print_endline "NFA :";
  print_string "states = ";
  State_set.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.states ;
  print_endline "";
  print_string "alphabets = ";
  Alphabet_set.iter (fun a -> print_string (Char.escaped a)) res.alphabets ;
  print_endline "";
  Transaction_set.iter (fun (a, b, c) ->
    match b with
    | Some k -> print_string ((string_of_int a) ^ " -> " ^ (Char.escaped k) ^ " -> " ^ (string_of_int c) ^ " " )
    | None -> print_string ((string_of_int a) ^ " ->" ^ "e"  ^ "-> " ^ (string_of_int c) ^ ", "))
  res.transactions;
  print_endline "";
  print_endline (string_of_int res.start_state);
  print_string "final_states = ";
  State_set.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.final_states ;
  print_endline ""


