open Core.Std
open Lexer

let file_path = "./regex-data.txt"

let test_equal regex =
  let input = Lexer.lexer regex in
  let str   = Lexer.tokens_to_string input in
  match regex = str with
  | true  -> print_string ""
  | false -> Printf.printf "Test error, input:%s, output%s \n"
                regex str

let lexer_test path = 
  let regexes = In_channel.read_lines path in
  List.iter regexes
    ~f:(fun re -> Printf.printf "testing %s\n" re ;test_equal re)

let () = 
  lexer_test file_path
  


