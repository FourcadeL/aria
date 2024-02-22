%{
open Audio
%}
%token END_OF_LINE START_COMMENT END_COMMENT TABUL
%token START_PAR END_PAR START_BRAC END_BRAC
%token COLON SEMI_COLON DOT COMA

%token AUDIO END

%token INSTRUMENT

%token BLOCK

%token SONG CHANNEL1 CHANNEL2 CHANNEL3 CHANNEL4

// 1 argument instructions
%token PLAYNOTE, WAIT, REPEATCOUNTERSET, CALLBLOCK, JUMPBLOCK

// 0 argument instructions
%token PLAYEMPTY, UNITWAIT, RESETSTACK, ENDBLOCK, CONDITIONNALRETURNTRACK, RETURNTRACK,
CONDITIONNALGLOBALRETURNTRACK, GLOBALRETURNTRACK, SETRETURNTRACK, SETENDSTATE 

// basenotes
%token C, CD, D, DD, E, F, FD, G, GD, A, AD, B

%token <string> IDENTIFIER
%token <int> INT
%token <int> OCTAVE
// %token <Audio.note> NOTE

%start start
%type <Audio.audio> start

%%
start:
|AUDIO instrument_list song_list END {Audio($2, $3)}



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
|WAIT INT {Wait($2)}
|UNITWAIT {Wait(1)}
|REPEATCOUNTERSET INT {RepeatCounterSet($2)}
|CALLBLOCK identifier {CallBlock($2)}
|JUMPBLOCK identifier {JumpBlock($2)}
|RESETSTACK {ResetStack}
|ENDBLOCK {EndBlock}
|CONDITIONNALRETURNTRACK {ConditionnalReturnTrack}
|RETURNTRACK {ReturnTrack}
|CONDITIONNALGLOBALRETURNTRACK {ConditionnalGlobalReturnTrack}
|GLOBALRETURNTRACK {GlobalReturnTrack}
|SETRETURNTRACK {SetReturnTrack}
|SETENDSTATE {SetEndState}

instruction_list:
|{[]} /*nothing is read*/
|instruction {[$1]}
|instruction instruction_list {$1::$2}

/*---------------------------------------------------------------*/
/*-----------------   Unit Block Declaration   ------------------*/
/*---------------------------------------------------------------*/
block:
|BLOCK identifier COLON instruction_list {Block($2, $4)}

block_list:
|{[]} /*nothing is read*/
|block {[$1]}
|block block_list {$1::$2}

/*---------------------------------------------------------------*/
/*----------------------Unit Song Declaration--------------------*/
/*---------------------------------------------------------------*/
song:
|SONG block_list CHANNEL1 identifier CHANNEL2 identifier CHANNEL3 identifier CHANNEL4 identifier {Song($2, $4, $6, $8, $10)}

song_list:
|{[]} /*nothing is read*/
|song {[$1]}
|song song_list {$1::$2}


/*---------------------------------------------------------------*/
/*----------------------     Identifier      --------------------*/
/*---------------------------------------------------------------*/
identifier: IDENTIFIER {Id($1)}

/*---------------------------------------------------------------*/
/*-------------------------     Note      -----------------------*/
/*---------------------------------------------------------------*/
note:
|base_note INT {Note($1, Oct($2))}

base_note:
|C {C}
|CD {Cd}
|D {D}
|DD {Dd}
|E {E}
|F {F}
|FD {Fd}
|G {G}
|GD {Gd}
|A {A}
|AD {Ad}
|B {B}