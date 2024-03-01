%{
open Audio
%}
%token END_OF_LINE START_COMMENT END_COMMENT TABUL
%token START_PAR END_PAR START_BRAC END_BRAC
%token COLON SEMI_COLON DOT COMA

%token AUDIO END

%token INSTRUMENT

%token SONG
%token BLOCK
%token CHANNEL1 CHANNEL2 CHANNEL3 CHANNEL4


// 1 argument instructions
%token PLAYNOTE, WAIT, REPEATCOUNTERSET, CALLBLOCK, JUMPBLOCK

// 0 argument instructions
%token PLAYEMPTY, UNITWAIT, RESETSTACK, ENDBLOCK, CONDITIONNALRETURNTRACK, RETURNTRACK,
CONDITIONNALGLOBALRETURNTRACK, GLOBALRETURNTRACK, SETRETURNTRACK, SETENDSTATE 

// basenotes
%token <string> BASENOTE

%token <string> IDENTIFIER
%token <int> INT
// %token <Audio.note> NOTE

%start start
%type <Audio.audio> start

%%
start:
|AUDIO instrument_list song_list block_list END {Audio($2, $3, $4)}



/*---------------------------------------------------------------*/
/*-----------------Unit Instruments Declaration------------------*/
/*---------------------------------------------------------------*/
instrument:
|INSTRUMENT identifier COLON INT SEMI_COLON INT SEMI_COLON INT SEMI_COLON INT SEMI_COLON {Instrument($2, $4, $6, $8, $10)}

instrument_list:
|{[]} /*nothing is read*/
|instrument {[$1]}
|instrument instrument_list {$1::$2}

/*---------------------------------------------------------------*/
/*--------------   Unit instruction Declaration   ---------------*/
/*---------------------------------------------------------------*/

instruction:
|note {PlayNote($1)}
|PLAYEMPTY {PlayEmpty}
|WAIT START_PAR INT END_PAR {Wait($3)}
|UNITWAIT {Wait(1)}
|REPEATCOUNTERSET START_PAR INT END_PAR {RepeatCounterSet($3)}
|CALLBLOCK START_PAR identifier END_PAR {CallBlock($3)}
|JUMPBLOCK START_PAR identifier END_PAR {JumpBlock($3)}
|RESETSTACK {ResetStack}
|ENDBLOCK {EndBlock}
|CONDITIONNALRETURNTRACK {ConditionnalReturnTrack}
|RETURNTRACK {ReturnTrack}
|CONDITIONNALGLOBALRETURNTRACK {ConditionnalGlobalReturnTrack}
|GLOBALRETURNTRACK {GlobalReturnTrack}
|SETRETURNTRACK {SetReturnTrack}
|SETENDSTATE {SetEndState}
|error {let start_pos = Parsing.rhs_start_pos 1 in
        let end_pos = Parsing.rhs_end_pos 1 in
        Printf.printf "unexpected instruction at Line %d : %d - %d\n"
        start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol) (end_pos.pos_cnum - end_pos.pos_bol);
        failwith ("unexpected token")}

instruction_list:
|{[]} /*nothing is read*/
|instruction {[$1]}
|instruction instruction_list {$1::$2}

/*---------------------------------------------------------------*/
/*-----------------   Unit Block Declaration   ------------------*/
/*---------------------------------------------------------------*/
block:
|BLOCK identifier COLON START_BRAC instruction_list END_BRAC {Block($2, $5)}

block_list:
|{[]} /*nothing is read*/
|block {[$1]}
|block block_list {$1::$2}

/*---------------------------------------------------------------*/
/*----------------------Unit Song Declaration--------------------*/
/*---------------------------------------------------------------*/
song:
|SONG identifier COLON CHANNEL1 identifier CHANNEL2 identifier CHANNEL3 identifier CHANNEL4 identifier {Song($2, Channel($5), Channel($7), Channel($9), Channel($11))}

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
