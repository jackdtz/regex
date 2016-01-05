type regex = 
  | Closure of regex
  | Char of char
  | Concatenation of regex * regex
  | Alternation of regex * regex
  | Epsilon


exception IllegalExpression of string

type token = 
  | End
  | Alphabet of char
  | Star
  | LParen
  | RParen
  | Pipe

let string_to_char_list str = 
  let rec helper i col = 
    if i < 0 then col else helper (i - 1) (str.[i] :: col)
  in
    helper (List.length str - 1) []

let tokenize str = 
  let char_list = string_to_char_list str in
  List.foldr
    (fun c acc ->
      if 'a' <= c && c <= 'z' then Char c :: acc else 
      if c = '(' then LParen :: acc else
      if c = ')' then RParen :: acc else
      if c = '|' then Pipe :: acc else
      if c = '*' then Star :: acc else
      failwith "Unknow token")
    [End]
    char_list


let lookahead token_list = 
  match token_list with
  | [] -> raise (IllegalExpression "lookahead")
  | hd :: tl -> (hd, tl)



(**
 *  P1 = P2 L1
 *  L1 = (|P2 L1) | E
 *  P2 = P3 L2
 *  L2 = P3 L2 | E
 *  P3 = P4L3
 *  L3 = *L3 | E
 *  P4 = terminal | (P1))
 *  terminal = a .. z
 **)

let rec parse_P1 l = 
  let (a1, l1) = parse_P2 in
  let (t, rest) = lookahead l1 in
  match t with
  | Pipe ->
      let (a2, l2) = parse_P2 rest in
        (Alternation (a1, a2), l2)
  | _ -> (a1, l1)

and parse_P2 l = 
  let (a1, l1) = parse_P3 in
  let (t, rest) = lookahead l1 in
  match t with
  | Alphabet a2 -> (Concatenation (a1, a2), rest)
  | _ -> (a1, l1)

and parse_P3 l = 
  let (a1, l1) = parse_P4 l in
  let (t, rest) = lookahead l1 in
  match t with
  | Star -> (Closure (Char a1), rest)
  | _ -> (a1, l1)

and parse_P4 l = 
  let (t1, rest1) = lookahead l in
  match t1 with
  | LParens -> 
     (let (a, rest2) = parse_P1 rest1 in
      let (t2, rest3) = lookahead rest2 in
      match t2 with
      | RParens -> (a, rest3)
      | _ -> raise (IllegalExpression "parens"))
  | Alphabet c -> 
      (Char c, rest1)
  | _ -> raise (IllegalExpression "alphabet")

  
