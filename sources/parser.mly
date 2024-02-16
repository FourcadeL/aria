%{
open Audio
%}

%token AUDIO END

%token INSTRUMENT

%token SONG CHANNEL1 CHANNEL2 CHANNEL3 CHANNEL4


%token <int> OCTAVE
%token <Audio.note> NOTE

%start start
%type <Audio.audio> start

%%
start:
|AUDIO instrument_list song_list END {Audio($2, $3)}



/*---------------------------------------------------------------*/
/*-----------------Unit Instruments Declaration------------------*/
/*---------------------------------------------------------------*/
instrument: /*todo*/


/*---------------------------------------------------------------*/
/*----------------------Unit Song Declaration--------------------*/
/*---------------------------------------------------------------*/
song:
|SONG CHANNEL1 instruction_list CHANNEL2 instruction_list CHANNEL3 instruction_list CHANNEL4 instruction_list {Song($3, $5, $7, $9)}

song_list:
|{[]} /*nothing is read*/
|song {[$1]}
|song song_list {$1::$2}