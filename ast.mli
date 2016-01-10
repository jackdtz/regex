type regex = 
  | Closure of regex
  | Char of char
  | Concatenation of regex * regex
  | Alternation of regex * regex

val regex_to_string : regex -> string

val string_to_char_list : string -> char list

val parse : string -> regex option

val run_test : unit -> unit


