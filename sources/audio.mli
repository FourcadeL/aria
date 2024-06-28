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
|Instrument of (string * int * int * int * int * int list)

(*------------- Song and instructions type ---------------*)
type instruction =
|PlayNote of note
|PlayEmpty
|VolumeSet of int
|InstrumentSet of int
|Wait of int
|RepeatCounterSet of int
|CallBlock of string
|JumpBlock of string
|ResetStack
|EndBlock
|ConditionnalReturnTrack
|ReturnTrack
|ConditionnalGlobalReturnTrack
|GlobalReturnTrack
|SetReturnTrack
|SetEndState

type channel =
|Channel of string

type block =
|Block of (string * (instruction list))

type song =
|Song of (string * channel * channel * channel * channel)






(*------------- Audio type ---------------*)
type audio =
Audio of (instrument list) * (song list) * (block list)


(*---------------------------------------------------------------*)
(*--------------------   generator function   -------------------*)
(*---------------------------------------------------------------*)

val basenote_of_string : string -> baseNote
val get_note_value : note -> int
val transpose_note : note -> int -> note

(*---------------------------------------------------------------*)
(*--------------------   string  function   ---------------------*)
(*---------------------------------------------------------------*)

val instruction_string : instruction -> string


(*---------------------------------------------------------------*)
(*--------------------   display function   ---------------------*)
(*---------------------------------------------------------------*)

val disp_audio : audio -> unit

val song_display : song -> unit

val block_display : block -> unit