open Ast
open Nfa
open Dfa
open Datatypes

let nfa_states = Lazy.gen_sequence 0
let dfa_states = Lazy.gen_sequence 0
let mdfa_states = Lazy.gen_sequence 0

type decision = | Accept | Reject

let string_to_dfa str n_lseq d_lseq m_lseq = 
  let (n, _) = Nfa.string_to_nfa str n_lseq in
  print_endline (Nfa.nfa_to_string n) ;
  Dfa.(minimize (nfa_to_dfa n d_lseq) m_lseq)

let move trans current tok = 
  Core.Std.Set.find_map trans
    ~f:(fun (sin, c, sout) ->
        match sin = current, c = tok with
        | true, true -> Some sout
        | _, _ -> None)


let simulate d input = 
  let rec helper d toks current =
    let dtrans = d.d_transactions in
    match toks with
    | [] -> 
        (match Core.Std.Set.mem d.d_final_states current with
        | false -> Reject
        | true -> Accept)
    | hd :: tl ->
        let next = move dtrans current hd in
        match next with
        | None -> Reject
        | Some next ->
            helper d tl next 
  in
  let toks = Ast.string_to_char_list input in
  helper d toks d.d_start_state


let () = 
  Printf.printf "%s\n" (Ast.string_of_parser_res (Ast.parse "a*(b|c)dde")) ;
  let d = string_to_dfa "a*(b|c)dde" nfa_states dfa_states mdfa_states in
  match simulate d "aaacdde" with
  | Accept -> print_endline "accept"
  | Reject -> print_endline "reject"
