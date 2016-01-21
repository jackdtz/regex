type order = Less | Equal | Greater

let int_compare x y = if x = y then Equal else if x < y then Less else Greater

let char_compare = int_compare
