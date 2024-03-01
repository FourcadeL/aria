open Audio

(*---------------------------------------------------------------*)
(*---------------------- checker functions ----------------------*)
(*---------------------------------------------------------------*)

val check_blocks_size : block list -> bool

val check_songs_pointers : song list -> block list -> unit