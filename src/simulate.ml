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
  Dfa.(minimize (nfa_to_dfa n d_lseq) m_lseq)

let move trans current tok = 
  D_Transition_map.find trans (current, tok)

let simulate d input = 
  let rec helper d toks current =
    let dtrans = d.d_transitions in
    match toks with
    | [] -> 
        (match State_set.mem d.d_final_states current with
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


let match_regex re_str str = 
  let d = string_to_dfa re_str nfa_states dfa_states mdfa_states in
  simulate d str

(*let () = *)
  (*let d = string_to_dfa "ac*(b|c)e" nfa_states dfa_states mdfa_states in*)
  (*Printf.printf "%s\n" (Dfa.dfa_to_string d);*)
  (*match simulate d "acccbe" with*)
  (*| Accept -> print_endline "accept"*)
  (*| Reject -> print_endline "reject"*)
