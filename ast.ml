exception IllegalExpression of string

type regex = 
  | Closure of regex
  | Char of char
  | Concatenation of regex * regex
  | Alternation of regex * regex

type token = 
  | End
  | Alphabet of char
  | Star
  | LParen
  | RParen
  | Pipe

let token_to_string tok = 
  match tok with
  | Alphabet c -> Char.escaped c
  | Star -> "*"
  | LParen -> "("
  | RParen -> ")"
  | Pipe -> "|"
  | End -> "END"

let rec regex_to_string r : string = 
  let addParens str = 
    "(" ^ str ^ ")"
  in
  match r with
  | Closure re -> 
      let res = (regex_to_string re) in 
      if String.length res > 1 then (addParens res) ^ "*" else res ^ "*"
  | Char c -> Char.escaped c
  | Concatenation (r1, r2) -> 
     (let res1 = regex_to_string r1 in
      let res2 = regex_to_string r2 in
      match ((String.length res1) > 1, (String.length res2) > 1) with
      | (true, true) -> (addParens res1) ^ (addParens res2)
      | (true, false) -> (addParens res1) ^ res2
      | (false, true) -> res1 ^ (addParens res2)
      | (false, false) -> res1 ^ res2)
  | Alternation (r1, r2) -> 
     (let res1 = regex_to_string r1 in
      let res2 = regex_to_string r2 in
      match ((String.length res1) > 1, (String.length res2) > 1) with
      | (true, true) -> (addParens res1) ^ "|" ^ (addParens res2)
      | (true, false) -> (addParens res1) ^ "|" ^ res2
      | (false, true) -> res1 ^ "|" ^ (addParens res2)
      | (false, false) -> res1 ^ "|" ^ res2)

let string_to_char_list (str:string) : char list = 
  let rec helper (i:int) (col:char list) = 
    if i < 0 then col else helper (i - 1) (str.[i] :: col)
  in
    helper ((String.length str) - 1) []

let tokenize str = 
  let char_list = string_to_char_list str in
  List.fold_right
    (fun c acc ->
      if 'a' <= c && c <= 'z' then Alphabet c :: acc else 
      if c = '(' then LParen :: acc else
      if c = ')' then RParen :: acc else
      if c = '|' then Pipe :: acc else
      if c = '*' then Star :: acc else
      if c = ' ' then acc else
      failwith "Unknow token")
    char_list
    [End]

let lookahead token_list = 
  match token_list with
  | [] -> raise (IllegalExpression "lookahead")
  | hd :: tl -> (hd, tl)

(**
 *
 * exp = term { "|" term}
 * term = factor { factor }
 * factor = element[*]
 * element = (exp) | a .. z
 *
 **)

let rec parse_exp (l : token list) : (regex * token list) = 
  let rec helper l term =
    let (t, rest) = lookahead l in
    match t with
    | Pipe -> 
        let (next, l1) = parse_term rest in
        helper l1 (Alternation (term, next))
    | _ -> (term, l)
  in
  let (a, l2) = parse_term l in
  helper l2 a

and parse_term (l : token list) : (regex * token list) = 
  let rec helper l factor = 
    let (t, rest) = lookahead l in
    match t with
    | Alphabet _ | LParen ->
        let (next, l1) = parse_factor l in
        helper l1 (Concatenation (factor, next))
    | _ -> (factor, l)
  in
  let (a, l2) = parse_factor l in
  helper l2 a

and parse_factor (l : token list) : (regex * token list) = 
  let (a, l1) = parse_element l in
  let (t, rest) = lookahead l1 in
  match t with
  | Star -> (Closure a, rest)
  | _ -> (a, l1)

and parse_element (l : token list) : (regex * token list) = 
  let (t, rest) = lookahead l in
  match t with
  | Alphabet c -> (Char c, rest)
  | LParen -> 
     (let (a, l1) = parse_exp rest in
      let (t1, l2) = lookahead l1 in
      match t1 with
      | RParen -> (a, l2)
      | _ -> raise (IllegalExpression "Unbalanced parentheses"))
  | _ -> raise (IllegalExpression "Unknown token")

let parse (str : string) : regex option = 
  let tok_list = tokenize str in
  if tok_list = [End] 
  then None
  else
    let (a, t) = parse_exp tok_list in 
    match t with
    | [End] -> Some a
    | _ -> raise (IllegalExpression "Parsing is not completed")

