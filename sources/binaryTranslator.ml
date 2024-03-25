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
  |PlayEmpty -> 0b01011111
  |VolumeSet(v) -> 0b01100000 + v
  |InstrumentSet(i) -> 0b01110000 + i
  |Wait(d) -> 0b11000000 + d
  |RepeatCounterSet(v) -> 0b10100000 + v
  |CallBlock(Id(_)) -> 0b10001011
  |JumpBlock(Id(_)) -> 0b10001010
  |ResetStack -> 0b10001001
  |EndBlock -> 0b10001000
  |ConditionnalReturnTrack -> 0b10000111
  |ReturnTrack -> 0b10000110
  |ConditionnalGlobalReturnTrack -> 0b10000101
  |GlobalReturnTrack -> 0b10000100
  |SetReturnTrack -> 0b10000001
  |SetEndState -> 0b10000000



let get_instruction_byte_size instruction =
  match instruction with
  |CallBlock(_) -> 2
  |JumpBlock(_) -> 2
  |_ -> 1