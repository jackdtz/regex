open Core.Std
open Sexplib
open Sexp


type state       = int with sexp, compare
type alphabet    = char with sexp, compare
type transaction = state * alphabet option * state with sexp, compare

type d_transaction = state * alphabet * state with sexp, compare


module State_set = Set.Make(
  struct
    type t = state with sexp
    let compare = Pervasives.compare
  end)

module Alphabet_set = Set.Make(
  struct
    type t = alphabet with sexp, compare
  end
)

module Transaction_set = Set.Make(
  struct
    type t = transaction with sexp, compare
  end
)

module D_Transaction_set = Set.Make(
  struct
    type t = d_transaction with sexp, compare
  end)

module Trans_in_states_set = Set.Make(
  struct
    type t = State_set.t * alphabet * State_set.t with sexp, compare
  end
)

module States_set = Set.Make(
  struct
    type t = State_set.t with sexp, compare
  end
)

module Dict = Map.Make(
  struct
    type t = State_set.t with sexp, compare
  end
)



type state_set = State_set.t
type states_set = States_set.t

type nfa = {
  states       : State_set.t ;
  alphabets    : Alphabet_set.t ;
  transactions : Transaction_set.t; 
  start_state  : state;
  final_states : State_set.t;
}


type dfa = {
  d_states       : State_set.t ;
  d_alphabets    : Alphabet_set.t;
  d_transactions : D_Transaction_set.t ;
  d_start_state  : state ;
  d_final_states : State_set.t;
}

type m_transaction = state * Alphabet_set.t * state with sexp, compare

module M_Transaction_set = Set.Make(
  struct 
    type t = m_transaction with sexp, compare
  end
)

type minimized_dfa = {
  m_states       : State_set.t ;
  m_alpbabets    : Alphabet_set.t ; 
  m_transactions : M_Transaction_set.t ;
  m_start_state  : state ;
  m_final_states : State_set.t ;
}



