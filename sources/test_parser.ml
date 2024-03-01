let lexbuf = Lexing.from_channel stdin


let _ =
let a = Parser.start (Lexer.read) lexbuf in
Audio.disp_audio a;
print_newline ()