{
open Parser
let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- {pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}
rule read = parse
| ' ' {read lexbuf}
| '|' {read lexbuf}
| '\t' {read lexbuf}
| '\n' {incr_linenum lexbuf; read lexbuf}
| '%' {comment lexbuf}
| '(' {START_PAR}
| ')' {END_PAR}
| '[' {START_BRAC}
| ']' {END_BRAC}
| ':' {COLON}
| ';' {SEMI_COLON}
| '.' {DOT}
| ',' {COMA}

| "audio" {AUDIO}
| "end" {END}

|"instrument" {INSTRUMENT}
|"song" {SONG}
|"block" {BLOCK}

|"ch1" {CHANNEL1}
|"ch2" {CHANNEL2}
|"ch3" {CHANNEL3}
|"ch4" {CHANNEL4}

(* Instruction contrôl *)
|"_" {PLAYEMPTY}
|"wait" {WAIT}
|"setVol" {SETVOLUME}
|"setInst" {SETINSTRUMENT}
|"setRep" {REPEATCOUNTERSET}
|"call" {CALLBLOCK}
|"jump" {JUMPBLOCK}
|"resetCall" {RESETSTACK}
|"e" {ENDBLOCK}
|"condReturn" {CONDITIONNALRETURNTRACK}
|"return" {RETURNTRACK}
|"condReturnGlobal" {CONDITIONNALGLOBALRETURNTRACK}
|"returnGlobal" {GLOBALRETURNTRACK}
|"setRet" {SETRETURNTRACK}
|"setEnd" {SETENDSTATE}

(*base note*)
|['A'-'G']['#''b']? {BASENOTE (Lexing.lexeme lexbuf)}

(* base types *)
|['a'-'z''A'-'Z']['a'-'z''A'-'Z']['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z']* {IDENTIFIER (Lexing.lexeme lexbuf)}
(*the quickfix for identifier and basenote confusion is that identifier are now required to be at least 3 non-alphabetic characters longs*)
|['0'-'9']+ {INT (int_of_string (Lexing.lexeme lexbuf))}

(* read error *)
| _ {let start_pos = Parsing.rhs_start_pos 1 in
        let end_pos = Parsing.rhs_end_pos 1 in
        Printf.printf "unexpected character \"%s\" at Line %d : %d - %d\n"
        (Lexing.lexeme lexbuf)
        start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol) (end_pos.pos_cnum - end_pos.pos_bol);
        failwith ("unexpected character")}


and comment = parse
| '\n' {incr_linenum lexbuf; read lexbuf} (*moves out of the comment lexing*)
| _ {comment lexbuf} (*stays in comment mode*)