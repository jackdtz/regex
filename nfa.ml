open Core.Std
open Datatypes

exception IllegalInput of string
module Lazy = Datatypes.Lazy_state

let states_union = State_set.union 
let alphabets_union = Alphabet_set.union 
let trans_union = Transaction_set.union


let make_nfa ~states:s ~alphabets:alp ~trans:t ~start:ss ~finals:f = 
  {
    states       = s; 
    alphabets    = alp; 
    transactions = t; 
    start_state  = ss; 
    final_states = f;
  }


let states_to_state states state edge  = 
  State_set.fold
    ~f:(fun acc x -> Transaction_set.add acc (x, edge, state))
    states
    ~init:Transaction_set.empty

let rec regex_to_nfa re lseq : (nfa * Lazy.t)= 
  match re with
  | `Char c -> handle_char c lseq
  | `Concatenation (e1, e2) -> handle_concat e1 e2 lseq
  | `Alternation (e1, e2) -> handle_altern e1 e2 lseq
  | `Closure e -> handle_closure e lseq

and handle_char c lseq = 
  let (s1, lrest1)  = Lazy.gen_state_num lseq in 
  let (s2, lrest2) = Lazy.gen_state_num lrest1 in
  let nfa_char = make_nfa 
    ~start:s1
    ~finals:(State_set.singleton s2)
    ~states:State_set.(add (singleton s1) s2)
    ~alphabets:(Alphabet_set.singleton c)
    ~trans:(Transaction_set.singleton (s1, Some c, s2))
  in (nfa_char, lrest2)

and handle_concat e1 e2 lseq = 
  let (nfa1, lseq1) = regex_to_nfa e1 lseq in
  let (nfa2, lseq2) = regex_to_nfa e2 lseq1 in
  let states1, states2 = nfa1.states, nfa2.states in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let final1 = nfa1.final_states in
  let nfa_concat = make_nfa
    ~start:nfa1.start_state
    ~finals:nfa2.final_states
    ~alphabets:(alphabets_union alps1 alps2)
    ~states:(states_union states1 states2)
    ~trans:(trans_union (trans_union trans1 trans2)
                        (states_to_state final1 nfa2.start_state None))
  in (nfa_concat, lseq2)

and handle_altern e1 e2 lseq = 
  let (nfa1, lseq1) = regex_to_nfa e1 lseq in
  let (nfa2, lseq2) = regex_to_nfa e2 lseq1 in
  let alps1, alps2 = nfa1.alphabets, nfa2.alphabets in
  let trans1, trans2 = nfa1.transactions, nfa2.transactions in
  let start1, start2 = nfa1.start_state, nfa2.start_state in
  let final1, final2 = nfa1.final_states, nfa2.final_states in
  let (s1, lseq3) = Lazy.gen_state_num lseq2 in
  let (s2, lseq4) = Lazy.gen_state_num lseq3 in
  let nfa_altern = make_nfa
    ~start:s1
    ~finals:(State_set.singleton s2)
    ~alphabets:(alphabets_union alps1 alps2)
    ~states:(states_union nfa1.states nfa2.states)
    ~trans:(Transaction_set.(
      (union trans1 trans2) |> union (states_to_state final1 s2 None)
                            |> union (states_to_state final2 s2 None)
                            |> Fn.flip add (s1, None, start1)
                            |> Fn.flip add (s1, None, start2)))
  in (nfa_altern, lseq4)

and handle_closure e lseq = 
  let (n, lseq1) = regex_to_nfa e lseq in
  let (s1, lseq2) = Lazy.gen_state_num lseq1 in
  let (s2, lseq3) = Lazy.gen_state_num lseq2 in
  let finals_to_start = 
    states_to_state n.final_states n.start_state None
  in
  let finals_to_s2 = 
    states_to_state n.final_states s2 None
  in
  let nfa_closure = make_nfa
    ~start:s1
    ~finals:(State_set.singleton s2)
    ~alphabets:n.alphabets
    ~states:State_set.(add (add n.states s1) s2)
    ~trans:Transaction_set.
      (union n.transactions finals_to_s2 
        |> union finals_to_start
        |> Fn.flip add (s1, None, s2)
        |> Fn.flip add (s1, None, n.start_state))
  in (nfa_closure, lseq3)

let nfa_to_string res =
  let module D = Datatypes in
  "NFA: \n" ^
  "States: " ^ (D.string_of_state_set res.states) ^ "\n" ^
  "Alphabets: " ^ (D.string_of_alps_set res.alphabets) ^ "\n" ^
  "Transaction: " ^ (D.string_of_trans_set res.transactions) ^ "\n" ^
  "Start state: " ^ (D.string_of_state res.start_state) ^ "\n" ^
  "Final states: " ^ (D.string_of_state_set res.final_states)

let string_to_nfa str lseq = 
  let tree = Ast.parse str in
    match tree with
    | Some regex -> regex_to_nfa regex lseq
    | None -> raise (IllegalInput "input string cannot be parsed")

