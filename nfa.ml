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

let states_to_state states state edge : transaction list = 
  List.fold_right 
    (fun x acc -> (x, edge, state) :: acc)
    states
    []

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
  let nfa1 = regex_to_nfa e1 in
  let nfa2 = regex_to_nfa e2 in
  {
    states = states_union nfa1.states nfa2.states ;
    alphabets = alphabets_union nfa1.alphabets nfa2.alphabets ;
    transactions = (states_to_state nfa1.final_states nfa2.start_state None) @
                      (nfa1.transactions @ nfa2.transactions) ;
    start_state = nfa1.start_state ;
    final_states = nfa2.final_states ;
  }

and handle_altern (e1 : regex) (e2 : regex) = 
  let nfa1 = regex_to_nfa e1 in
  let nfa2 = regex_to_nfa e2 in
  let start1 = nfa1.start_state in
  let start2 = nfa2.start_state in
  let finals1 = nfa1.final_states in
  let finals2 = nfa2.final_states in
  let s1 = gen_state_num () in
  let s2 = gen_state_num () in
  {
    states = s1 :: s2 :: (states_union nfa1.states nfa2.states);
    alphabets = (alphabets_union nfa1.alphabets nfa2.alphabets);
    transactions = (s1, None, start1) :: (s1, None, start2) ::
      ((states_to_state finals1 s2 None) @ (states_to_state finals2 s2 None) @
        nfa1.transactions @ nfa2.transactions);
    start_state = s1;
    final_states = [s2];
  }

and handle_closure (e : regex) : nfa = 
  let s1 = gen_state_num () in 
  let s2 = gen_state_num () in
  let nfa1 = regex_to_nfa e in
  let finals_to_start = 
    states_to_state nfa1.final_states nfa1.start_state None
  in
  let finals_to_s2 = 
    states_to_state nfa1.final_states s2 None
  in
  {
    states =  s1 :: s2 :: nfa1.states ;
    alphabets = nfa1.alphabets ;
    transactions = (s1, None, nfa1.start_state) :: (s1, None, s2) ::
      (finals_to_start @ finals_to_s2 @ nfa1.transactions) ;
    start_state = s1 ;
    final_states = [s2] ;
  }

let test1 = Concatenation (Char 'a', Char 'd')
let test2 = Closure (Char 'a')

let expect_test1 = {
  states = [0; 1; 2; 3];
  alphabets = ['a'; 'd'];
  transactions = [(0, Some 'a', 1); (1, None, 2); (2, Some 'b', 3)];
  start_state = 0;
  final_states = [3];
}

let expect_test2 = {
  states = [0; 1; 2; 3] ;
  alphabets =  ['a'];
  transactions = [(0, None, 1); (1, Some 'c', 2); (2, None, 3); (0, None, 3); (2, None, 1)];
  start_state = 0;
  final_states = [3];
}

let test_nfa () = 
  assert(regex_to_nfa test1 = expect_test1);
  assert(regex_to_nfa test2 = expect_test2)

let nfa_to_string res =
  print_endline "";
  print_string "states = ";
  List.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.states ;
  print_endline "";
  print_string "alphabets = ";
  List.iter (fun a -> print_string (Char.escaped a)) res.alphabets ;
  print_endline "";
  List.iter (fun (a, b, c) ->
    match b with
    | Some k -> print_string ((string_of_int a) ^ " -> " ^ (Char.escaped k) ^ " -> " ^ (string_of_int c) ^ " " )
    | None -> print_string ((string_of_int a) ^ " ->" ^ "e"  ^ "-> " ^ (string_of_int c) ^ ", "))
  res.transactions;
  print_endline "";
  print_endline (string_of_int res.start_state);
  print_string "final_states = ";
  List.iter (fun a -> print_string ((string_of_int a) ^ " ")) res.final_states ;


