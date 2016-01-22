
type d_transaction = state * alphabet * state


module State_set = Set.Make(
  struct
    type t = int
    let compare = Pervasives.compare
  end)

module Alphabet_set = Set.Make(
  struct
    type t = char
    let compare = Pervasives.compare 
  end
)

module Transaction_set = Set.Make(
  struct
    type t = transaction
    let compare (t1 : t)  (t2 : t) = 
      match t1, t2 with
      | (x1, Some c1, x2), (y1, Some c2, y2) ->
           (match Pervasives.compare x1 y1,
                 Pervasives.compare c1 c2,
                 Pervasives.compare x2 y2 
           with
           | 0, 0, 0 -> 0
           | 0, 0, 1 | 0, 1, 0 | 0, 1, 1
           | 1, 0, 0 | 1, 0, 1 | 1, 1, 1 -> 1
           | _ -> - 1)
      | (x1, None, x2), (y1, None, y2) ->
          (match Pervasives.compare x1 y1,
                Pervasives.compare x2 y2
          with
          | 0, 0 -> 0
          | 0, 1 | 1, 0 | 1, 1 -> 1
          | _ -> - 1)
      | (x1, Some c, x2), (y1, None, y2) -> Char.code c
      | (x1, None, x2), (y1, Some c, y2) -> Char.code c
  end
)

module Trans_in_states_set = Set.Make(
  struct
    type t = State_set.t * alphabet * State_set.t
    let compare (t1 : t) (t2 : t) = 
      match t1, t2 with
      | (x1, c1, x2), (y1, c2, y2) ->
           (match State_set.compare x1 y1,
                  Pervasives.compare c1 c2,
                  State_set.compare x2 y2 
           with
           | 0, 0, 0 -> 0
           | 0, 0, 1 | 0, 1, 0 | 0, 1, 1
           | 1, 0, 0 | 1, 0, 1 | 1, 1, 1 -> 1
           | _ -> - 1)
  end
)

module States_set = Set.Make(
  struct
    type t = State_set.t
    let compare = State_set.compare
  end
)

module Dict = Map.Make(
  struct
    type t = State_set.t
    let compare = State_set.compare
  end
)



type dfa = {
  d_states : state_set ;
  d_alphabets : alphabet list;
  d_transactions : d_transaction list;
  d_start_state : state ;
  d_final_states : state_set ;
}
