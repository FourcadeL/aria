%{
open Audio
%}
%token END_OF_LINE START_COMMENT END_COMMENT TABUL
%token START_PAR END_PAR START_BRAC END_BRAC
%token COLON SEMI_COLON DOT COMA

%token AUDIO END

%token INSTRUMENT

%token SONG CHANNEL1 CHANNEL2 CHANNEL3 CHANNEL4

%token <String> IDENTIFIER
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
/*----------------------Unit Song Declaration--------------------*/
/*---------------------------------------------------------------*/
song:
|SONG block_list CHANNEL1 identifier CHANNEL2 instruction_list CHANNEL3 instruction_list CHANNEL4 instruction_list {Song($3, $5, $7, $9)}

song_list:
|{[]} /*nothing is read*/
|song {[$1]}
|song song_list {$1::$2}


/*---------------------------------------------------------------*/
/*----------------------     Identifier      --------------------*/
/*---------------------------------------------------------------*/
identifier: IDENTIFIER {Id($1)}

