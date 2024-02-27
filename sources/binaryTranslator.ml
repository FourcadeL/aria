open Audio

exception UndefinedNote

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

let get_note_value note =
  let Note(baseNote, Oct(octave)) = note in
  if octave >= 2 && octave <= 9 then
    12 * (octave - 2) + get_basenote_offset baseNote
  else
    raise UndefinedNote 

let get_instruction_value instruction =
  match instruction with
  |PlayNote(n) -> get_note_value n
  |PlayEmpty -> 0b01010100
  |Wait(d) -> 0b11000000 + d
  |RepeatCounterSet(v) -> 0b10100000 + v
  |CallBlock(Id(_)) -> 0b10001011
  |JumpBlock(Id(_)) -> 0b10001010
  |ResetStack -> ()
  |EndBlock -> ()
  |ConditionnalReturnTrack -> ()
  |ReturnTrack -> ()
  |ConditionnalGlobalReturnTrack -> ()
  |GlobalReturnTrack -> ()
  |SetReturnTrack -> ()
  |SetEndState -> 0b00000001