open Lexer
open Parser
open Main
open Printf
open Arg


let run prog =
    try let run_lexer = Lexing.from_channel prog in
            main lexer_main run_lexer
    with Parsing.Parse_error -> failwith "There is a problem with the syntax." ;;

let param = ref stdin in
  let setProg p = param := open_in p in
    let usage = "mysplinterpreter < PROGRAM_FILE" in
      parse [] setProg usage ;
      let parsedProg = run !param in
        let output = eval parsedProg in
          print output ;
          print_newline() ;
          flush stdout
