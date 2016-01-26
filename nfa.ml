open Core.Std
open Datatypes

module Lazy = Datatypes.Lazy_state


let states_union = State_set.union 
let alphabets_union = Alphabet_set.union 
let trans_union = Transaction_set.union

let nfa_state_list = Datatypes.Lazy_state.gen_sequence (-1)


let states_to_state states state edge  = 
  State_set.fold
    ~f:(fun acc x -> Transaction_set.add acc (x, edge, state))
    states
    ~init:Transaction_set.empty

let rec regex_to_nfa re = 
  match re with
  | `Char c -> handle_char c
  | `Concatenation (e1, e2) -> handle_concat e1 e2
  | `Alternation (e1, e2) -> handle_altern e1 e2
  | `Closure e -> handle_closure e

and handle_char c  = 
  let s1  = Datatypes.gen_state_num () in 
  let s2 = Datatypes.gen_state_num () in
  {
    states = State_set.(singleton s1 |> Fn.flip add s2) ;
    alphabets = Alphabet_set.singleton c ;
    transactions = Transaction_set.singleton (s1, Some c, s2);
    start_state = s1;
    final_states = State_set.(singleton s2);
  }

and handle_concat e1 e2 = 
  let nfa1, nfa2 = regex_to_nfa e1, regex_to_nfa e2 in
  let states1, states2 = nfa1.states, nfa2.states in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let final1 = nfa1.final_states in
  {
    states = states_union states1 states2 ;
    alphabets = alphabets_union alps1 alps2;
    transactions = trans_union (trans_union trans1 trans2) 
                               (states_to_state final1 nfa2.start_state None) ;
    start_state = nfa1.start_state ;
    final_states = nfa2.final_states ;
  }

and handle_altern e1 e2 = 
  let nfa1, nfa2 = regex_to_nfa e1, regex_to_nfa e2 in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let start1, start2 = nfa1.start_state, nfa2.start_state in
  let final1, final2 = nfa1.final_states, nfa2.final_states in
  let s1 = Datatypes.gen_state_num () in
  let s2 = Datatypes.gen_state_num () in
  let flip = Fn.flip in
  {
    states = states_union nfa1.states nfa2.states ;
    alphabets = alphabets_union alps1 alps2 ;
    transactions = Transaction_set.
        (union trans1 trans2 |> flip union (states_to_state final1 s2 None) 
                             |> flip union (states_to_state final2 s2 None)
                             |> flip add (s1, None, start1)
                             |> flip add (s1, None, start2));
    start_state = s1;
    final_states = State_set.(singleton s2);
  }

and handle_closure e = 
  let s1 = Datatypes.gen_state_num () in 
  let s2 = Datatypes.gen_state_num () in
  let n = regex_to_nfa e in
  let finals_to_start = 
    states_to_state n.final_states n.start_state None
  in
  let finals_to_s2 = 
    states_to_state n.final_states s2 None
  in
  let flip = Fn.flip in
  {
    states = State_set.(n.states |> flip add s1 |> flip add s2) ;
    alphabets = n.alphabets ;
    transactions = Transaction_set.
      (union n.transactions finals_to_s2 
        |> flip union finals_to_start
        |> flip add (s1, None, s2)
        |> flip add (s1, None, n.start_state)) ;
    start_state = s1 ;
    final_states = State_set.(singleton s2);
  }

let string_of_state_set states = 
  print_string "{ " ;
  State_set.iter ~f:(fun a -> print_string ((string_of_int a) ^ " ")) states ;
  print_string " }" 

let string_of_alphabet_set alphabets = 
  print_string "alphabets = ";
  Alphabet_set.iter ~f:(fun a -> print_string (Char.escaped a)) alphabets ;
  print_endline ""

let string_of_transactions transactions = 
  Transaction_set.iter ~f:(fun (a, b, c) ->
    match b with
    | Some k -> print_string ((string_of_int a) ^ " -> " ^ (Char.escaped k) ^ " -> " ^ (string_of_int c) ^ " " )
    | None -> print_string ((string_of_int a) ^ " ->" ^ "e"  ^ "-> " ^ (string_of_int c) ^ ", "))
  transactions;
  print_endline ""

let string_of_start start_state= 
  print_endline (string_of_int start_state)

let string_of_finals final_states = 
  print_string "final_states = ";
  State_set.iter ~f:(fun a -> print_string ((string_of_int a) ^ " ")) final_states ;
  print_endline ""

let nfa_to_string res =
  print_endline "NFA :";
  string_of_state_set res.states;
  string_of_alphabet_set res.alphabets;
  string_of_transactions res.transactions;
  string_of_start res.start_state;
  string_of_finals res.final_states


