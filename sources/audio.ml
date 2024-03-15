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
(*-----------------string description functions------------------*)
(*---------------------------------------------------------------*)

let baseNote_string baseNote =
  match baseNote with
  |C -> "C"
  |Cd -> "C#"
  |D -> "D"
  |Dd -> "D#"
  |E -> "E"
  |F -> "F"
  |Fd -> "F#"
  |G -> "G"
  |Gd -> "G#"
  |A -> "A"
  |Ad -> "A#"
  |B -> "B"

let note_string note =
  match note with
  |Note(base, Oct(n)) -> (baseNote_string base) ^ (string_of_int n)

let rec wait_string n buff =
  match n with
  |0 -> buff
  |1 -> buff ^ "."
  |n -> wait_string (n-1) (buff ^ ". ")

let instruction_string instruction =
  match instruction with
  |PlayNote(note) -> note_string note
  |PlayEmpty -> "_"
  |Wait(n) -> "Set wait (" ^ string_of_int n ^ ")" ^ (wait_string n "")
  |RepeatCounterSet(n) -> "RepeatSet(" ^ (string_of_int n) ^ ")"
  |CallBlock(Id(id)) -> "Call(" ^ id ^ ")"
  |JumpBlock(Id(id)) -> "Jump(" ^ id ^ ")"
  |ResetStack -> "Clear"
  |EndBlock -> "End"
  |ConditionnalReturnTrack -> "TrackRepeatCond"
  |ReturnTrack -> "TrackRepeat"
  |ConditionnalGlobalReturnTrack -> "GlobalRepeatCond"
  |GlobalReturnTrack -> "GlobalRepeat"
  |SetReturnTrack -> "SetReturnTrack"
  |SetEndState -> "SetEnd"

(*---------------------------------------------------------------*)
(*--------------------   generator function   -------------------*)
(*---------------------------------------------------------------*)
let basenote_of_string str =
match str with
|"C" -> C
|"C#" -> Cd
|"Db" -> Cd
|"D" -> D
|"D#" -> Dd
|"Eb" -> Dd
|"E" -> E
|"F" -> F
|"F#" -> Fd
|"Gb" -> Fd
|"G" -> G
|"G#" -> Gd
|"Ab" -> Gd
|"A" -> A
|"A#" -> Ad
|"Bb" -> Ad
|"B" -> B
|_ -> failwith "unknown note match"

(*---------------------------------------------------------------*)
(*--------------------   display function   ---------------------*)
(*---------------------------------------------------------------*)
let instruction_display i =
  Printf.printf " %s " (instruction_string i)

let block_display b =
  let Block(Id(id), instructions) = b in
  Printf.printf "Block : %s\n\t" id;
  List.iter instruction_display instructions;
  Printf.printf "\n";;

let song_display s =
  let Song(blocks, Channel(Id(ch1)), Channel(Id(ch2)), Channel(Id(ch3)), Channel(Id(ch4))) = s in
  Printf.printf "Song:\n\t|CH1 : %s\n\t|CH2 : %s\n\t|CH3 : %s\n\t|CH4 : %s\n" ch1 ch2 ch3 ch4;;

let disp_audio a =
  let Audio(instruments, songs, blocks) = a in
  Printf.printf "instruments display not implemented yet\n";
  List.iter song_display songs;
  List.iter block_display blocks;
  


