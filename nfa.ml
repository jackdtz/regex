open Ast

type state       = int
type alphabet    = char
type transaction = state * alphabet option * state

type nfa = {
  states       : state list;
  alphabets    : alphabet list;
  transactions : transaction list; 
  start_state  : state;
  final_states : state list;
}

let state_num = ref (- 1) 

let gen_state_num () = 
  state_num := !state_num + 1;
  !state_num

let rec union_helper l1 l2 = 
  List.fold_right
    (fun x acc -> if List.mem x l2 then acc else x :: acc)
    l1
    l2
let states_union s1 s2 = 
  union_helper s1 s2

let alphabets_union a1 a2 = 
  union_helper a1 a2   

let state_helper nfa1 nfa2 op =
  let finals = nfa1.final_states in 
  let start = nfa2.start_state in 
  let start_char = List.hd nfa2.alphabets in
  List.fold_right 
    (fun s acc -> 
      match op with
      | Some _ -> (s, Some start_char, start) :: acc
      | None -> (s, None, start) :: acc) 
    finals
    []

let concat_nfas nfa1 nfa2 : transaction list =
  state_helper nfa1 nfa2 (Some 1)

let altern_nfas nfa1 nfa2 : transaction list = 
  state_helper nfa1 nfa2 None

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
    states = [s1; s2];
    alphabets = [c];
    transactions = [(s1, Some c, s2)];
    start_state = s1;
    final_states = [s2];
  }

and handle_concat (e1 : regex) (e2 : regex) : nfa = 
  let nfa2 = regex_to_nfa e2 in
  let nfa1 = regex_to_nfa e1 in
  let new_states = states_union nfa1.states nfa2.states in
  let new_alphabets = alphabets_union nfa1.alphabets nfa2.alphabets in
  let new_start = nfa1.start_state in
  let new_final_states = nfa2.final_states in
  let new_transactions = (concat_nfas nfa1 nfa2) @ nfa1.transactions @ nfa2.transactions in
  {
    states       = new_states;
    alphabets    = new_alphabets;
    transactions = new_transactions;
    start_state  = new_start;
    final_states = new_final_states;
  }

and handle_altern (e1 : regex) (e2 : regex) = 
  let nfa2 = regex_to_nfa e2 in
  let nfa1 = regex_to_nfa e1 in
  let new_states = states_union nfa1.states nfa2.states in
  let new_alphabets = alphabets_union nfa1.alphabets nfa2.alphabets in
  let new_start = nfa1.start_state in
  let new_final_states = nfa2.final_states in
  let new_transactions = (altern_nfas nfa1 nfa2) @ nfa1.transactions @ nfa2.transactions in
  {
    states       = new_states;
    alphabets    = new_alphabets;
    transactions = new_transactions;
    start_state  = new_start;
    final_states = new_final_states;
  }

and handle_closure (e : regex) : nfa = 
  let s1 = gen_state_num () in 
  let s2 = gen_state_num () in
  let nfa1 = regex_to_nfa e in
  let finals_to_start = 
    List.fold_right 
      (fun x acc -> (x, None, nfa1.start_state) :: acc)
      nfa1.final_states 
      []
  in
  let finals_to_s2 = 
    List.fold_right 
      (fun x acc -> (x, None, s2 ) :: acc)
      nfa1.final_states 
      []
  in
  {
    states =  s1 :: s2 :: nfa1.states ;
    alphabets = nfa1.alphabets ;
    transactions = (s1, None, nfa1.start_state) :: (s1, None, s2) ::
      (finals_to_start @ finals_to_s2 @ nfa1.transactions) ;
    start_state = s1 ;
    final_states = [s2] ;
  }






