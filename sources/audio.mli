(*identifier used for block and instrument declaration*)
type identifier =
|Id of string

(*octave type - denote an octave number*)
type octave =
|Oct of int

(*baseNote type - denote a note*)
type baseNote =
|C
|Cd
|D
|Dd
|E
|F
|Fd
|G
|Gd
|A
|Ad
|B

(*not type - the full note characterisation*)
type note =
|Note of baseNote * octave

(*------------- Instrument type ---------------*)
type instrument =
|Instrument of (identifier * int * int * int * int)

(*------------- Song and instructions type ---------------*)
type instruction =
|PlayNote of note
|PlayEmpty
|Wait of int
|RepeatCounterSet of int
|CallBlock of identifier
|JumpBlock of identifier
|ResetStack
|EndBlock
|ConditionnalReturnTrack
|ReturnTrack
|ConditionnalGlobalReturnTrack
|GlobalReturnTrack
|SetReturnTrack
|SetEndState

type channel =
|Channel of identifier

type block =
|Block of (identifier * (instruction list))

type song =
|Song of (identifier * channel * channel * channel * channel)






(*------------- Audio type ---------------*)
type audio =
Audio of (instrument list) * (song list) * (block list)


(*---------------------------------------------------------------*)
(*--------------------   generator function   -------------------*)
(*---------------------------------------------------------------*)

val basenote_of_string : string -> baseNote


(*---------------------------------------------------------------*)
(*--------------------   display function   ---------------------*)
(*---------------------------------------------------------------*)

val disp_audio : audio -> unit

val song_display : song -> unit

val block_display : block -> unit