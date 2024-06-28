open Audio


let get_instruction_value instruction =
  match instruction with
  |PlayNote(n) -> get_note_value n
  |PlayEmpty -> 0b01011111
  |VolumeSet(v) -> 0b01100000 + v
  |InstrumentSet(i) -> 0b01110000 + i
  |Wait(d) -> 0b11000000 + d
  |RepeatCounterSet(v) -> 0b10100000 + v
  |CallBlock(_) -> 0b10001011
  |JumpBlock(_) -> 0b10001010
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


let get_audio_5bitsigned_modifier_value modifier =
  match modifier with
  |0 -> 0x00
  |1 -> 0x01
  |2 -> 0x02
  |3 -> 0x03
  |4 -> 0x04
  |5 -> 0x05
  |6 -> 0x06
  |7 -> 0x07
  |8 -> 0x08
  |9 -> 0x09
  |10 -> 0x0A
  |11 -> 0x0B
  |12 -> 0x0C
  |13 -> 0x0D
  |14 -> 0x0E
  |15 -> 0x0F
  |(-1) -> 0x1F
  |(-2) -> 0x1E
  |(-3) -> 0x1D
  |(-4) -> 0x1C
  |(-5) -> 0x1B
  |(-6) -> 0x1A
  |(-7) -> 0x19
  |(-8) -> 0x18
  |(-9) -> 0x17
  |(-10) -> 0x16
  |(-11) -> 0x15
  |(-12) -> 0x14
  |(-13) -> 0x13
  |(-14) -> 0x12
  |(-15) -> 0x11
  |(-16) -> 0x10
  |_ -> 0x00