open Nfa
open Dfa

let nfa_state_list = Lazy.gen_sequence 0
let dfa_state_list = Lazy.gen_sequence 0
let mdfa_state_list = Lazy.gen_sequence 0

let () = 
   let (n, _) = Nfa.string_to_nfa "a*" nfa_state_list in
    let d1  = Dfa.nfa_to_dfa n dfa_state_list in
    dfa_to_string d1;
    let d2 = Dfa.minimize d1 mdfa_state_list in
    dfa_to_string d2 
