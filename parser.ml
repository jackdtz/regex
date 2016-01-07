

type regex = 
  | Closure of regex
  | Char of char
  | Concatenation of regex * regex
  | Alternation of regex * regex
  (*| Epsilon*)


exception IllegalExpression of string

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
  match r with
  | Closure re -> (regex_to_string re) ^ "*"
  | Char c -> Char.escaped c
  | Concatenation (r1, r2) -> (regex_to_string r1) ^ (regex_to_string r2)
  | Alternation (r1, r2) -> (regex_to_string r1) ^ "|" ^ (regex_to_string r2)
  (*| Epsilon -> ""*)

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
      failwith "Unknow token")
    char_list
    [End]


let lookahead token_list = 
  match token_list with
  | [] -> raise (IllegalExpression "lookahead")
  | hd :: tl -> (hd, tl)



(**
 * exp     = concat "|" exp | concat
 * concat  = term concat | term
 * term    = element* | element
 * element = (exp) | a .. z 
 *
 * exp = concat factor1
 * factor1 = "|" exp | e
 * concat = term factor2
 * factor2 = concat | e
 * term = element factor3
 * factor3 = * | e
 * element = (exp) | a .. z
 *
 * S = T X
 * X = "|" S | E
 * T = F Y 
 * Y = T | E
 * F = U Z
 * Z = * | E
 * U = (S) | a .. z
 *
 **)

let rec parse_S (l : token list) : (regex * token list) = 
  let (a1, l1) = parse_T l in
  let (t, rest) = lookahead l1 in 
  match t with
  | Pipe ->                                   (* S = T | S*)
      let (a2, l2) = parse_S rest in
      (Alternation (a1, a2), l2)
  | _ -> (a1, l1)                             (* S = T *)

and parse_T (l : token list) : (regex * token list) = 
  let (a1, l1) = parse_F l in
  let (t, rest) = lookahead l1 in 
  match t with
  | Alphabet c -> (Char c, rest)
  | LParen -> 
     (let (a, l1) = parse_S rest in
      let (t1, l2) = lookahead l1 in
      match t1 with
      | RParen -> (a, l2)
      | _ -> raise (IllegalExpression "Unbalanced parentheses"))
  | _ -> (a1, l1)


and parse_F (l : token list) : (regex * token list) = 
  let (a1, l1) = parse_U l in 
  let (t, rest) = lookahead l1 in 
  match t with
  | Star -> (Closure a1, rest)
  | _ -> (a1, l1)

and parse_U l = 
  let (t, rest) = lookahead l in
  match t with
  | Alphabet c -> (Char c, rest)
  | LParen -> 
     (let (a, l1) = parse_S rest in
      let (t1, l2) = lookahead l1 in
      match t1 with
      | RParen -> (a, l2)
      | _ -> raise (IllegalExpression "Unbalanced parentheses"))
  | _ -> raise (IllegalExpression "Unknown token")



let parse str : regex = 
  let tok_list = tokenize str in
  print_string "Input token list = " ;
  List.iter (fun c -> print_string (" " ^ (token_to_string c))) tok_list;
  print_endline "" ;

  let (a, t) = parse_S tok_list in
  List.iter (fun c -> print_string (" " ^ (token_to_string c))) t;
  print_endline "";
  if t <> [End] then raise (IllegalExpression "last token is not END") else 
  a


let _ = 
  parse "a(b|c)*"
  (*let (a, l) = parse_S [Alphabet 'a'; LParen; Alphabet 'b'; Pipe; Alphabet 'c'; RParen; Star; End] in *)
  (*l = [End] *)
  
