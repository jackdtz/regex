
module State_set = Set.Make(
  struct
    type t = int
    let compare = Pervasives.compare
  end)

module States_set = Set.Make(
  struct
    type t = State_set.t
    let compare = State_set.compare
  end

)
