open Nfa

exception IllegalInput of string

let make_nfa (str : string) : nfa = 
  let tree = Ast.parse str in
  match tree with
  | Some regex -> Nfa.regex_to_nfa regex
  | None -> raise (IllegalInput "input string cannot be parsed")

let reduce f init seq = 
  match seq with
  | [] -> init
  | hd :: tl -> reduce f (f hd init) tl

let match_re (re : str) (str : string) : bool = 
  let tokens = string_to_char_list str in
  let {
    states       = s;
    alphabets    = a;
    transactions = t;
    start_state  = st;
    final_states = fs;
  } = make_nfa str in 
  let rec helper start finals transactions tokens next in
  match tokens with
  | [] -> if List.mem finals ans then true else false
  | hd :: tl ->
      let pass_e_next = List.fold_left 
        (fun acc (in, c, out) ->
          match c with 
          | Some c -> acc 
          | None -> if in = hd then out else acc









