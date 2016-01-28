open Core.Std
open Sexplib
open Sexp

type state       = int with sexp, compare
type alphabet    = char with sexp, compare
type transaction = state * alphabet option * state with sexp, compare
type d_transaction = state * alphabet * state with sexp, compare


module Lazy_state = struct
  type t = Cons of int * (unit -> t)

  let rec gen_sequence n = Cons (n, fun () -> gen_sequence (n + 1))

  let head (Cons (n, _)) = n
  
  let tail (Cons (_, tl)) = tl ()

  let gen_state_num lseq = (head lseq, tail lseq)
end


let string_of_set ?(sep=",") set elt_to_string  = 
  Set.elements set
    |> List.map ~f:elt_to_string
    |> String.concat ~sep


(*******************************************************)

module State_set = Set.Make(
  struct
    type t = state with sexp
    let compare = Pervasives.compare
  end)

let string_of_state = string_of_int
let string_of_state_set set : Core.Std.String.t = 
  "{" ^ string_of_set set string_of_state ^ "}"

(*******************************************************)

module Alphabet_set = Set.Make(
  struct
    type t = alphabet with sexp, compare
  end
)

let string_of_alps = Char.escaped
let string_of_alps_set set = 
  string_of_set set string_of_alps 

(*******************************************************)
module Transaction_set = Set.Make(
  struct
    type t = transaction with sexp, compare
  end
)

let string_of_trans_set set = 
  let string_of_trans (sin, c, sout) =
    (string_of_state sin) ^ "->" ^ 
    (match c with
    | None -> "e"
    | Some ch -> string_of_alps ch
    ) 
    ^ "->" ^
    (string_of_state sout)
  in
  string_of_set set string_of_trans 


(*******************************************************)

module D_Transaction_set = Set.Make(
  struct
    type t = d_transaction with sexp, compare
  end)

let string_of_dtrans_set set = 
  let string_of_dtrans (sin, c, sout) =
    (string_of_state sin) ^ "->" ^ 
    string_of_alps c ^ "->" ^
    (string_of_state sout)
  in
  string_of_set set string_of_dtrans 

(*******************************************************)

module Trans_in_states_set = Set.Make(
  struct
    type t = State_set.t * alphabet * State_set.t with sexp, compare
  end
)

let string_of_setTrans_set (set : Trans_in_states_set.t) = 
  let string_of_set_trans (set1, c, set2) =
    (string_of_state_set set1) ^ "->" ^
    (string_of_alps c) ^ "->" ^
    (string_of_state_set set2)
  in
  string_of_set set string_of_set_trans

(*******************************************************)

module States_set = Set.Make(
  struct
    type t = State_set.t with sexp, compare
  end
)

let string_of_states_set (set : States_set.t) = 
  string_of_set set string_of_state_set

(*******************************************************)

module Dict = Map.Make(
  struct
    type t = State_set.t with sexp, compare
  end
)

let string_of_dict dict = 
  let string_of_dict_elt (states, state)= 
    "(key: " ^ (string_of_state_set states) ^ ", " ^ 
    "data: " ^ (string_of_state state) ^ ")"
  in
  string_of_set dict string_of_dict_elt

(*******************************************************)


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


