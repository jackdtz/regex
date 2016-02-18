open Core.Std

exception IllegalRegex of string
exception IllegalInput of string

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


type token = 
  | ALPHABET of char
  | ESCAPE   of char
  | NUMBER   of int
  | LPAREN                (* (  *)
  | RPAREN                (* )  *) 
  | LCURLY                (* {  *)
  | RCURLY                (* }  *) 
  | LSQURE                (* [  *)
  | RSQURE                (* ]  *)
  | STAR                 (* *  *)
  | PIPE                  (* |  *)
  | DOT                   (* .  *)
  | BEGIN                 (* ^  *)
  | END                   (* $  *)
  | QUESTION              (* ?  *)
  | PLUS                  (* +  *)
  | WORD_BOUND            (* \b *)
  | WORD_EXCL             (* \B *)
  | NUM_BOUND             (* \d *)
  | NUM_EXCL              (* \D *)
  | NEWLINE               (* \n *)
  | SPACE                 (* \s *)
  | SPACE_EXCL            (* \S *)
  | TAB                   (* \t *)
  | EXCL_BEGIN            (* [^ *)
  | ALPHANUMB             (* \w *)
  | ALPHANUMB_EXCL        (* \W *)
  | SEP                   (* -  *)
  | COMMA                 (* ,  *)
  | BACKSLASH             (* \  *)


type state = 
  | START
  | INSQUARE
  | INSLASH
  | DONE

let is_int s = Char.is_digit s

let char_to_int c = (Char.to_int c) - 48

let char_to_string c = Char.escaped c

let is_alphabet s = Char.is_alpha s

let string_to_char_list str = 
  let rec helper (i:int) (col:char list) = 
    if i < 0 then col else helper (i - 1) (str.[i] :: col)
  in
    helper ((String.length str) - 1) []

let slash_lst = ['b';'B';'d';'D';'n';'t';'s';'S';'w';'W']

let meta_chars = ['['; ']'; '\\'; '$'; '.'; '|'; 
                  '?'; '*'; '+'; '{'; '}'; '('; ')']

let char_to_token c = 
  match c with
  | '['   -> LSQURE
  | ']'   -> RSQURE
  | '('   -> LPAREN
  | ')'   -> RPAREN
  | '{'   -> LCURLY
  | '}'   -> RCURLY
  | '*'   -> STAR
  | '|'   -> PIPE
  | '?'   -> QUESTION
  | '+'   -> PLUS
  | '^'   -> BEGIN
  | '$'   -> END
  | '.'   -> DOT
  | '-'   -> SEP
  | ','   -> COMMA
  | '\\'  -> BACKSLASH
  | _ -> 
      if (is_alphabet c)
      then ALPHABET c
      else if (is_int c)
      then NUMBER (char_to_int c)
      else raise (IllegalInput (char_to_string c))

let slash_c_to_token c =
  match c with
  | 'b'   -> WORD_BOUND
  | 'B'   -> WORD_EXCL
  | 'd'   -> NUM_BOUND
  | 'D'   -> NUM_EXCL
  | 'n'   -> NEWLINE
  | 's'   -> SPACE
  | 'S'   -> SPACE_EXCL
  | 't'   -> TAB
  | 'w'   -> ALPHANUMB
  | 'W'   -> ALPHANUMB_EXCL
  | '['   -> LSQURE
  | ']'   -> RSQURE
  | '\\'  -> BACKSLASH
  | '$'   -> END
  | '.'   -> DOT
  | '|'   -> PIPE
  | '?'   -> QUESTION
  | '*'   -> STAR
  | '+'   -> PLUS
  | '{'   -> LCURLY
  | '}'   -> RCURLY
  | '('   -> LPAREN
  | ')'   -> RPAREN
  | _   -> raise (IllegalInput (char_to_string c))

let token_to_string token = 
  match token with
  | ALPHABET c      -> Char.to_string c 
  | NUMBER   i      -> string_of_int i
  | ESCAPE   c      -> "\\" ^ Char.to_string c 
  | BACKSLASH       -> "\\"       (* \  *)
  | LPAREN          -> "("        (* (  *)
  | RPAREN          -> ")"        (* )  *) 
  | LCURLY          -> "{"        (* {  *)
  | RCURLY          -> "}"        (* }  *) 
  | LSQURE          -> "["        (* [  *)
  | RSQURE          -> "]"        (* ]  *)
  | STAR            -> "*"        (* *  *)
  | PIPE            -> "|"        (* |  *)
  | DOT             -> "."        (* .  *)
  | BEGIN           -> "^"        (* ^  *)
  | END             -> "$"        (* $  *)
  | QUESTION        -> "?"        (* ?  *)
  | PLUS            -> "+"        (* +  *)
  | WORD_BOUND      -> "\b"       (* \b *)
  | WORD_EXCL       -> "\\B"      (* \B *)
  | NUM_BOUND       -> "\\d"      (* \d *)
  | NUM_EXCL        -> "\\D"      (* \D *)
  | NEWLINE         -> "\\n"      (* \n *)
  | SPACE           -> "\\s"      (* \s *)
  | SPACE_EXCL      -> "\\S"      (* \S *)
  | TAB             -> "\\t"      (* \t *)
  | EXCL_BEGIN      -> "[^"       (* [^ *)
  | ALPHANUMB       -> "\\w"      (* \w *)
  | ALPHANUMB_EXCL  -> "\\W"      (* \W *)
  | SEP             -> "-"        (* -  *)
  | COMMA           -> ","        (* ,  *)

let tokens_to_string tokens =
  List.map tokens ~f:token_to_string
  |> String.concat 

let rec lexer_helper buffer cur_state acc = 
  match buffer with
  | [] -> List.rev acc
  | cur_char :: rest ->
      match cur_state with
      | DONE -> lexer_helper buffer START acc
      | START ->
          (match cur_char with
          | '['  -> lexer_helper rest INSQUARE acc
          | '\\' -> lexer_helper rest INSLASH acc
          | _    -> lexer_helper rest DONE ((char_to_token cur_char) :: acc))
      | INSQUARE ->
          (match cur_char with
          | '^' -> lexer_helper rest DONE (EXCL_BEGIN :: acc)
          | _   -> lexer_helper buffer DONE (LSQURE :: acc))
      | INSLASH ->
          (let mem = List.mem in
          match mem slash_lst cur_char, mem meta_chars cur_char with
          | true, false -> 
              lexer_helper rest DONE ((slash_c_to_token cur_char) :: acc)
          | false, true -> 
              lexer_helper rest DONE ((slash_c_to_token cur_char) :: BACKSLASH :: acc)

          | _ -> raise (IllegalRegex ("Unknow \\" ^ Char.escaped cur_char)))

let lexer str = 
  let buffer = string_to_char_list str in
  lexer_helper buffer START [] 



