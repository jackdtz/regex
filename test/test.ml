open Ast
open Core.Std
open Simulate
open Lexer

let parser_equal regex_str y  = 
  let res = Ast.parse regex_str in
  if res = y
  then print_string ""
  else print_string
        ("Parser test FAILED, input regex:  " ^ regex_str ^ "\n" ^
        "Expect: " ^ (Ast.string_of_parser_res y) ^ "\n" ^
        "Actual: " ^ (Ast.string_of_parser_res res))
  
let test_parser () = 
  parser_equal "a(b|c)*"  
                (Some (`Concatenation 
                        (`Char 'a', 
                        (`Closure (`Alternation (`Char 'b', `Char 'c')))))) ;
  parser_equal "a"  (Some (`Char 'a')) ;
  parser_equal "a|b"  (Some (`Alternation (`Char 'a', `Char 'b')));
  parser_equal "aab"  (Some (`Concatenation 
                              (`Concatenation (`Char 'a', `Char 'a'), 
                               `Char 'b')));
  parser_equal "a*(a|b)"  (Some (`Concatenation 
                                  (`Closure (`Char 'a'),   
                                   `Alternation (`Char 'a', `Char 'b'))));
  parser_equal ""  None;
  parser_equal "ad(b|c)*e" (Some (`Concatenation 
                                    (`Concatenation 
                                      ((`Concatenation (`Char 'a', `Char 'd')), 
                                        `Closure (`Alternation (`Char 'b', `Char 'c'))), 
                                    `Char 'e')));
  parser_equal "a*(b|c)*d"  (Some (`Concatenation 
                                        ((`Concatenation 
                                          (`Closure (`Char 'a'), 
                                          (`Closure (`Alternation (`Char 'b', `Char 'c'))))),
                                        (`Char 'd'))))

let get_full_paths base_path file_lst = 
  List.map file_lst
  ~f:(fun file_str -> base_path ^ "/" ^ file_str)

let read_file file_path = 
  In_channel.read_lines file_path

let test_regex regex datas = 
  List.iter datas
  ~f:(fun data -> 
    match Simulate.match_regex regex data with
    | Accept -> print_string ""
    | Reject -> 
        Printf.printf "Test failed, regex: %s match string: %s\n"
                      regex data
  )
  

let test () = 
  let base_path = "./test/testfiles" in
  let all_files = get_full_paths base_path (Sys.ls_dir base_path) in
  let data_lst = List.map all_files read_file in
  List.iter data_lst
  ~f:(fun data_set -> 
      let regex, datas = List.hd_exn data_set, List.tl_exn data_set in
      test_regex regex datas);
  print_endline "Finished"


let () = test ()








