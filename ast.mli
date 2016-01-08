type regex = 
  | Closure of regex
  | Char of char
  | Concatenation of regex * regex
  | Alternation of regex * regex
  | Empty

val regex_to_string : regex -> string

val parse : string -> regex

val run_test : unit -> unit


