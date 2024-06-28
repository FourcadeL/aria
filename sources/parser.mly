%{
open Audio
open Ast
%}
%token END_OF_LINE START_COMMENT END_COMMENT TABUL
%token START_PAR END_PAR START_BRAC END_BRAC START_CBRAC END_CBRAC
%token COLON SEMI_COLON DOT COMA

%token AUDIO END

%token INSTRUMENT

%token SONG
%token BLOCK
%token CHANNEL1 CHANNEL2 CHANNEL3 CHANNEL4



// 0 argument instructions
%token PLAYEMPTY
// block ast symbols
%token BLKAST_REPEAT BLKAST_TRANSPOSE BLKAST_WITHVOLUME
BLKAST_WITHINSTRUMENT BLKAST_LOOP BLKAST_CALL BLKAST_JUMP


// basenotes
%token <string> BASENOTE

%token <string> IDENTIFIER
%token <int> INT
// %token <Audio.note> NOTE

%start start
%type <Ast.globalAst> start

%%
start:
|AUDIO instrument_list song_list block_list END {Ast($2, $3, $4)}
|error {let start_pos = Parsing.rhs_start_pos 1 in
        let end_pos = Parsing.rhs_end_pos 1 in
        Printf.printf "unexpected encounter at Line %d : %d - %d\n"
        start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol) (end_pos.pos_cnum - end_pos.pos_bol);
        failwith ("unexpected error")}



/*---------------------------------------------------------------*/
/*-----------------Unit Instruments Declaration------------------*/
/*---------------------------------------------------------------*/
instrument:
|INSTRUMENT identifier COLON INT COMA INT COMA INT COMA INT SEMI_COLON volume_list SEMI_COLON {Instrument($2, RegisterInstrument($4, $6, $8, $10, $12))}

volume_list:
|{[]} /*nothing is read*/
|INT {[$1]}
|INT COMA volume_list {$1::$3}

instrument_list:
|{[]} /*nothing is read*/
|instrument {[$1]}
|instrument instrument_list {$1::$2}

/*---------------------------------------------------------------*/
/*--------------   Unit instruction Declaration   ---------------*/
/*---------------------------------------------------------------*/
instruction:
|note                                   {Note($1)}
|PLAYEMPTY                              {BlankNote}
|DOT                                    {EmptyPulse}
|IDENTIFIER                             {BlockId(Id($1))}
|BLKAST_REPEAT START_PAR INT END_PAR START_CBRAC instruction_ast END_CBRAC
                                        {Repeat($3, $6)}
|BLKAST_TRANSPOSE START_PAR INT END_PAR START_CBRAC instruction_ast END_CBRAC
                                        {Transpose($3, $6)}
|BLKAST_WITHVOLUME START_PAR INT END_PAR START_CBRAC instruction_ast END_CBRAC
                                        {WithVolume($3, $6)}
|BLKAST_WITHINSTRUMENT START_PAR INT END_PAR START_CBRAC instruction_ast END_CBRAC
                                        {WithInstrument($3, $6)}
|BLKAST_LOOP START_CBRAC instruction_ast END_CBRAC
                                        {Loop($3)}
|BLKAST_CALL START_CBRAC instruction_ast END_CBRAC
                                        {Call($3)}
|BLKAST_JUMP START_CBRAC instruction_ast END_CBRAC
                                        {Jump($3)}
|error {let start_pos = Parsing.rhs_start_pos 1 in
        let end_pos = Parsing.rhs_end_pos 1 in
        Printf.printf "unexpected instruction at Line %d : %d - %d\n"
        start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol) (end_pos.pos_cnum - end_pos.pos_bol);
        failwith ("unexpected token")}

instruction_ast:
// |{} /*nothing is read*/
|instruction {$1}
|instruction instruction_ast {Seq($1, $2)}

/*---------------------------------------------------------------*/
/*--------------------   Block AST parsing   --------------------*/
/*---------------------------------------------------------------*/
block:
|BLOCK identifier COLON START_BRAC instruction_ast END_BRAC {Block($2, $5)}

block_list:
|{[]} /*nothing is read*/
|block {[$1]}
|block block_list {$1::$2}

/*---------------------------------------------------------------*/
/*----------------------Unit Song Declaration--------------------*/
/*---------------------------------------------------------------*/
song:
|SONG identifier COLON CHANNEL1 identifier CHANNEL2 identifier CHANNEL3 identifier CHANNEL4 identifier {Song($2, PointersSong($5, $7, $9, $11))}

song_list:
|{[]} /*nothing is read*/
|song {[$1]}
|song song_list {$1::$2}


/*---------------------------------------------------------------*/
/*-------------------------     Note      -----------------------*/
/*---------------------------------------------------------------*/
note:
|BASENOTE INT {Note(Audio.basenote_of_string $1, Oct($2))}

/*---------------------------------------------------------------*/
/*----------------------     Identifier      --------------------*/
/*---------------------------------------------------------------*/
identifier: IDENTIFIER {Id($1)}
