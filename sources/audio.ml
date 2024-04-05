exception UndefinedNote


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
|Instrument of (string * int * int * int * int)

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
  |VolumeSet(v) -> "Set Volume(" ^ string_of_int v ^ ")"
  |InstrumentSet(i) -> "Set Instrument(" ^ string_of_int i ^ ")"
  |Wait(n) -> "Set wait (" ^ string_of_int n ^ ")" ^ (wait_string n "")
  |RepeatCounterSet(n) -> "RepeatSet(" ^ (string_of_int n) ^ ")"
  |CallBlock(id) -> "Call(" ^ id ^ ")"
  |JumpBlock(id) -> "Jump(" ^ id ^ ")"
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

let get_basenote_offset basenote =
  match basenote with
  | C -> 0
  | Cd -> 1
  | D -> 2
  | Dd -> 3
  | E -> 4
  | F  -> 5
  | Fd -> 6
  | G -> 7
  | Gd -> 8
  | A -> 9
  | Ad -> 10
  | B -> 11

  let get_basenote_from_offset offset =
    match offset with
    | 0 -> C
    | 1 -> Cd
    | 2 -> D
    | 3 -> Dd
    | 4 -> E
    | 5 -> F
    | 6 -> Fd
    | 7 -> G
    | 8 -> Gd
    | 9 -> A
    | 10 -> Ad
    | 11 -> B
    | _ -> raise UndefinedNote

let get_note_value note =
  let Note(baseNote, Oct(octave)) = note in
  if octave >= 2 && octave <= 9 then
    12 * (octave - 2) + get_basenote_offset baseNote
  else
    raise UndefinedNote 

let get_note_from_value v =
  let base = v mod 12 in
  let oct = (v / 12) + 2 in
  Note(get_basenote_from_offset base, Oct(oct))

let transpose_note note offset =
  get_note_from_value ((get_note_value note) + offset)

(*---------------------------------------------------------------*)
(*--------------------   display function   ---------------------*)
(*---------------------------------------------------------------*)
let instruction_display i =
  Printf.printf " %s " (instruction_string i)

let block_display b =
  let Block(id, instructions) = b in
  Printf.printf "Block : %s\n\t" id;
  List.iter instruction_display instructions;
  Printf.printf "\n";;

let song_display s =
  let Song(blocks, Channel(ch1), Channel(ch2), Channel(ch3), Channel(ch4)) = s in
  Printf.printf "Song:\n\t|CH1 : %s\n\t|CH2 : %s\n\t|CH3 : %s\n\t|CH4 : %s\n" ch1 ch2 ch3 ch4;;

let disp_audio a =
  let Audio(instruments, songs, blocks) = a in
  Printf.printf "instruments display not implemented yet\n";
  List.iter song_display songs;
  List.iter block_display blocks;
  


