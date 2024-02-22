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

|"Ch1" {CHANNEL1}
|"Ch2" {CHANNEL2}
|"Ch3" {CHANNEL3}
|"Ch4" {CHANNEL4}

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

(* note contrôl *)
|"C" {C}
|"C#" {CD}
|"D" {D}
|"D#" {DD}
|"E" {E}
|"F" {F}
|"F#" {FD}
|"G" {G}
|"G#" {GD}
|"A" {A}
|"A#" {AD}
|"B" {B}

|['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z']* {IDENTIFIER (Lexing.lexeme lexbuf)}
|['0'-'9']+ {INT (int_of_string (Lexing.lexeme lexbuf))}
(* | '{'[^'}']*'}' {read lexbuf}

| "program" {PROGRAM}

| "var" {VAR}
| "begin" {BEGIN}
| "end" {END}

| "function" {FUNCTION}
| "procedure" {PROCEDURE}

| "integer" {INTEGER}
| "boolean" {BOOLEAN}

| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "while" {WHILE}
| "do" {DO}

| "and" {AND}
| "or" {OR}
| "not" {NOT}

| ":=" {AFFECT}


| "new" {NEW}
| "array" {ARRAY}
| "of" {OF}
| "type" {TYPE}



| "false" {BOOL false}
| "true" {BOOL true} *)
