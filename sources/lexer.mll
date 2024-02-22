{
open Parser
}
rule read = parse
| ' ' {read lexbuf}
| '\t' {read lexbuf}
| '\n' {read lexbuf}
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
|"w" {UNITWAIT}
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

(* base note contrôl *)
|"C" {C}
|"D" {D}
|"E" {E}
|"F" {F}
|"G" {G}
|"A" {A}
|"B" {B}
|"#" {DIESE}

|['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z']* {IDENTIFIER (Lexing.lexeme lexbuf)}
|['0'-'9']+ {INT (int_of_string (Lexing.lexeme lexbuf))}
(* | '{'[^'}']*'}' {read lexbuf}

| "new" {NEW}
| "array" {ARRAY}
| "of" {OF}
| "type" {TYPE}

*)
