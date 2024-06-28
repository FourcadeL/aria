open Audio

(*---------------------------------------------------------------*)
(*-------------------- translation function ---------------------*)
(*---------------------------------------------------------------*)

val get_instruction_value : instruction -> int

val get_instruction_byte_size : instruction -> int

val get_audio_5bitsigned_modifier_value : int -> int
