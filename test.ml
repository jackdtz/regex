open Ast
open Nfa
open Re

let test_string_to_char_list () = 
  assert((Ast.string_to_char_list "a*(b|c)*") = ['a'; '*'; '('; 'b'; '|'; 'c'; ')'; '*']);
  assert((Ast.string_to_char_list "a*(b | c)*" = ['a'; '*'; '('; 'b'; ' '; '|'; ' ';  'c'; ')'; '*']));
  assert((Ast.string_to_char_list "") = [])


let test_parser () = 
  assert(Ast.parse "a(b|c)*" = 
        Some (Concatenation (Char 'a', (Closure (Alternation (Char 'b', Char 'c'))))));
  assert(Ast.parse "a" = Some (Char 'a'));
  assert(Ast.parse "a|b" = Some (Alternation (Char 'a', Char 'b')));
  assert(Ast.parse "aab" = Some (Concatenation (Char 'a', Concatenation (Char 'a', Char 'b'))));
  assert(Ast.parse "a*(a|b)" = Some (Concatenation (Closure (Char 'a'),   Alternation (Char 'a', Char 'b'))));
  assert(Ast.parse "" = None);
  assert(Ast.parse "a*(b|c)*d" = Some (Concatenation 
                                        ((Concatenation 
                                          (Closure (Char 'a'), 
                                          (Closure (Alternation (Char 'b', Char 'c'))))),
                                        (Char 'd'))))

let run_ast_tests () =
  test_string_to_char_list () ;
  test_parser ();
  print_endline "ALL TEST CASES PASSED"


let run_test () = 
  run_ast_tests ()

let _ = run_test ()

