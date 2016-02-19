open Core.Std


(*
 * http://www.boost.org/doc/libs/1_50_0/libs/regex/doc/html/boost_regex/syntax/basic_extended.html
 * http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html
 * regex    = term ( "|" term)*
 * term     = ["^"] concat
 *          | slash_metacharacter
 *          | concat ["$"]
 * concat   = factor factor*
 * factor   = element [*]
 *          | element [+]
 *          | element [?]
 *          | element "{" int* "}"
 *          | element "{" int* "," "}"
 *          | element "{" int* "," int* "}"
 * element  = "(" group ")"
 * group    = "[" alphnum "-" alphnum "]"
 *          | "[" alphnum "-" alphnum alphnum "-" alphnum "]"
 *          | "[^" alphnum "-" alphnum "]"
 *          | "[^" alphnum "-" alphnum alphnum "-" alphnum "]"
 *          | escaped_char
 * metacharacter = ...
 * escaped_char = ...
 * int = 0 .. 9
 * alphnum = 0 .. 9 a .. z A .. Z
 *
 *)

let lookahead = function
  | hd :: tl -> (hd, tl)
  | _        -> failwith "error here"

let rec parse_regex tokens = 
  let rec helper lst term = 
    let (next, rest) = lookahead lst in
    match next with
    | `PIPE -> let (term1, rest1) = parse_term rest in
        (`Alternation (term, term1), rest1)
    | _ -> (term, lst)
  in
  let (term1, rest_toks) = parse_term tokens in
  helper rest_toks term1

and parse_term tokens = 
  let (next_tok, rest_toks) = lookahead tokens in
  match next_tok with
  | `BEGIN -> let (factor, rest1) = parse_factor rest_toks in
      (`BEGIN_WITH factor, rest1)
  | `BACKSLASH meta_char -> 

type range = 
  | Range_int  of int * int
  | Range_char of char * char

type repeat_range = 
  | Repeat_n of int
  | Repeat_n_more of int
  | Repeat_m_n of int * int

type regex = 
  | Char of char
  | Int of int
  | Closure of regex
  | Concatenation of regex * regex
  | Alternation of regex * regex
  | Range of range
  | Exclude_range of range
  | Dot 
  | Plus of regex
  | Question of regex
  | Begin of regex
  | End of regex
  | Repeat_range of regex * repeat_range

