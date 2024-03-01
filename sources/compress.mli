(*---------------------------------------------------------------*)
(*---------------------- compress functions ---------------------*)
(*---------------------------------------------------------------*)

open Audio

val merge_waits : instruction list -> instruction list

val compress_block : block -> block

val compress_audio : audio -> audio