open Parser


let _ = 
  Parser.parser "a(b|c)*" = 
    Concatenation(Char 'a, Closure (Alternation(Char 'b, Char 'c)))
