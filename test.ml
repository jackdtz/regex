open Ast
open Nfa
open Re

let parser_equal (regex_str : string) (y : regex option) = 
  let res = Ast.parse regex_str in
  if res = y
  then print_string ""
  else print_string
        ("Parser test FAILED, input regex:  " ^ regex_str ^ "\n" ^
        "Expect: " ^ (Ast.string_of_parser_res y) ^ "\n" ^
        "Actual: " ^ (Ast.string_of_parser_res res))
  
let test_parser () = 
  parser_equal "a(b|c)*"  
                (Some (Concatenation (Char 'a', (Closure (Alternation (Char 'b', Char 'c')))))) ;
  parser_equal "a"  (Some (Char 'a')) ;
  parser_equal "a|b"  (Some (Alternation (Char 'a', Char 'b')));
  parser_equal "aab"  (Some (Concatenation (Concatenation (Char 'a', Char 'a'), Char 'b')));
  parser_equal "a*(a|b)"  (Some (Concatenation (Closure (Char 'a'),   Alternation (Char 'a', Char 'b'))));
  parser_equal ""  None;
  parser_equal "ad(b|c)*e" (Some (Concatenation 
                                    (Concatenation 
                                      ((Concatenation (Char 'a', Char 'd')), 
                                        Closure (Alternation (Char 'b', Char 'c'))), 
                                    Char 'e')));
  parser_equal "a*(b|c)*d"  (Some (Concatenation 
                                        ((Concatenation 
                                          (Closure (Char 'a'), 
                                          (Closure (Alternation (Char 'b', Char 'c'))))),
                                        (Char 'd'))))


let _ = test_parser ()

