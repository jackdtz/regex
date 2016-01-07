open Parser

let test_parser () = 
  assert(Parser.parse "a(b|c)*" = 
        (Concatenation (Char 'a', (Closure (Alternation (Char 'b', Char 'c'))))));
  assert(Parser.parse "a" = Char 'a');
  assert(Parser.parse "a|b" = Alternation (Char 'a', Char 'b'));
  assert(Parser.parse "aab" = Concatenation (Char 'a', Concatenation (Char 'a', Char 'b')));
  assert(Parser.parse "a*(a|b)" = Concatenation (Closure (Char 'a'),   Alternation (Char 'a', Char 'b')))


let _ = 
  test_parser ()
