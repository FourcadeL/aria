open Audio

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_instruction out_channel instruction =
  match instruction with
  |PlayNote(n) -> () (*TODO cas spécial de la note*)
  |PlayEmpty -> () (*TODO tous les cas d'instruction en fait*)
  |Wait(d) -> ()
  |RepeatCounterSet(v) -> ()
  |CallBlock(Id(id)) -> ()
  |JumpBlock(Id(id)) -> ()
  |ResetStack -> ()
  |EndBlock -> ()
  |ConditionnalReturnTrack -> ()
  |ReturnTrack -> ()
  |ConditionnalGlobalReturnTrack -> ()
  |GlobalReturnTrack -> ()
  |SetReturnTrack -> ()
  |SetEndState -> ()


let rec write_instruction_list outChannel instruction_list =
  match instruction_list with
  |[] -> ()
  |h::q -> write_instruction outChannel h;
          write_instruction_list outChannel q;;

let write_block block outChannel sectionCounter =
  Printf.fprintf outChannel "\tSECTION \"songblock_%s\", ROMX, ALIGN[8]\n" (string_of_int sectionCounter);
  let Block(Id(id), instruction_list) = block in
  Printf.fprintf outChannel "%s:\n" id;
  write_instruction_list outChannel instruction_list;;




let write_instrument outChannel instr = 
  let Instrument(_, v1, v2, v3, v4) = instr in
  Printf.fprintf outChannel "\t DB $%x, $%x, $%x, $%x\n" v1 v2 v3 v4

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_file_header outChannel = () (*nothing to write for now*)


let write_instruments outChannel instrs =
  Printf.fprintf outChannel "\tSECTION \"instruments sheet\", ROMX, ALIGN[6]\n_instruments_sheet:\n";
  List.iter (write_instrument outChannel) instrs

let write_songs_channel_pointers outChannel songs = ()

let write_songs_blocks outChannel songs = ()

(*---------------------------------------------------------------*)
(*-----------------global file output function ------------------*)
(*---------------------------------------------------------------*)

let output_rgbds_file audio fileName =
  let outChannel = open_out (fileName^".asm") in
  let Audio(instruments, songs) = audio in
  write_file_header outChannel; (*writes import definitions*)
  write_instruments outChannel instruments; (*writes the instrument section in file*)
  write_songs_channel_pointers outChannel songs; (*writes the songs channels pointers to the output files*)
  write_songs_blocks outChannel songs; (*writes the songs blocks to the output file*)
  close_out outChannel;;


(*
------------ FINAL OUTPUT FILE HAS FOLLOWING STRUCTURE : ---------------

HEADER

INSTRUMENT SECTION :
  SECTION "instruments sheet", ROMX, ALIGN[6] (aligned as required by engine)
_instruments_sheet:
  DB $XX, $XX, $XX, $XX
  DB $XX, $XX, $XX, $XX
  DB $XX, $XX, $XX, $XX
  DB $XX, $XX, $XX, $XX
  DB $XX, $XX, $XX, $XX
  DB $XX, $XX, $XX, $XX <- instruments values

  SECTION "songs", ROMX (no need to align)
_song_name:
  DB HIGH(channel1_block_addr), HIGH(channel2_block_addr), HIGH(channel3_block_addr), HIGH(channel4_block_addr)
_song_name_2:
  DB HIGH(channel1_block_addr), HIGH(channel2_block_addr), HIGH(channel3_block_addr), HIGH(channel4_block_addr)
  .
  .
  .
  
  SECTION "songblock_1", ROMX, ALIGN[8] (8 aligned as required by the engine)
_block_name_1:
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb

  SECTION "songblock_2", ROMX, ALIGN[8] (8 aligned as required by the engine)
_block_name_2:
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
*)