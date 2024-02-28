open RgbdsFileGenerator

let lexbuf = Lexing.from_channel stdin

let _ =
let a = Parser.start (Lexer.read) lexbuf in
output_rgbds_file a "test.asm"