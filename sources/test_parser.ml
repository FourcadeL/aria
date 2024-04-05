let lexbuf = Lexing.from_channel stdin


let _ =
let a = Parser.start (Lexer.read) lexbuf in
Ast.disp_ast a;
print_newline ()