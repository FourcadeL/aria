open Audio
open BinaryTranslator

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_instruction outChannel instruction =
  let v = get_instruction_value instruction in
  let _ = Printf.fprintf outChannel "\tDB $%0*X\n" 2 v in
  match instruction with
  |CallBlock(Id(id)) -> Printf.fprintf outChannel "\tDB HIGH(%s)\n" id (*special cases of 2 bytes instructions*)
  |JumpBlock(Id(id)) -> Printf.fprintf outChannel "\tDB HIGH(%s)\n" id
  |_ -> ()


let rec write_instruction_list outChannel instruction_list =
  match instruction_list with
  |[] -> ()
  |h::q -> write_instruction outChannel h;
          write_instruction_list outChannel q;;

let write_block block outChannel sectionCounter =
  Printf.fprintf outChannel "\tSECTION \"songblock_%s\", ROMX, ALIGN[8]\n" (string_of_int sectionCounter);
  let Block(Id(id), instruction_list) = block in
  Printf.fprintf outChannel "%s:\n" id;
  write_instruction_list outChannel instruction_list;
  Printf.fprintf outChannel "\n";;


let write_instrument outChannel instr = 
  let Instrument(_, v1, v2, v3, v4) = instr in
  Printf.fprintf outChannel "\t DB $%x, $%x, $%x, $%x\n" v1 v2 v3 v4

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_file_header outChannel =
  let infoHeader = "; this song file was generate using ARIA\n; for any informations please see :\n; https://github.com/FourcadeL/aria\n; \n; This aims to streamline music composition for the GB-engine tracker automaton\n; implemented by : https://github.com/FourcadeL/GB-Engine\n; \n; \n; ----------------\n; --- song file --\n; ----------------\n\n" in
  Printf.fprintf outChannel "%s" infoHeader;;


let write_instruments outChannel instrs =
  Printf.fprintf outChannel "\tSECTION \"instruments sheet\", ROMX, ALIGN[6]\n_instruments_sheet:\n";
  List.iter (write_instrument outChannel) instrs

let write_songs_channel_pointers outChannel songs =
  let rec aux song_list counter =
    match song_list with
    |[] -> ()
    |Song(_, Channel(Id(id1)), Channel(Id(id2)), Channel(Id(id3)), Channel(Id(id4)))::q ->
      Printf.fprintf outChannel "song_%s:\n" (string_of_int counter);
      Printf.fprintf outChannel "\tDB HIGH(%s), HIGH(%s), HIGH(%s), HIGH(%s)\n" id1 id2 id3 id4;
      aux q (counter + 1);
  in
  Printf.fprintf outChannel "\tSECTION \"songs lookup\", ROMX\n";
  aux songs 0;;

let write_blocks outChannel blocks =
  let rec aux block_list counter =
    match block_list with
    |[] -> ()
    |h::q -> write_block h outChannel counter; aux q (counter + 1)
  in
  aux blocks 0;;

(*---------------------------------------------------------------*)
(*-----------------global file output function ------------------*)
(*---------------------------------------------------------------*)

let output_rgbds_file audio fileName =
  let outChannel = open_out (fileName^".asm") in
  let Audio(instruments, songs, blocks) = audio in
  write_file_header outChannel; (*writes import definitions*)
  Printf.fprintf outChannel "\n\n";
  write_instruments outChannel instruments; (*writes the instrument section in file*)
  Printf.fprintf outChannel "\n\n";
  write_songs_channel_pointers outChannel songs; (*writes the songs channels pointers to the output files*)
  Printf.fprintf outChannel "\n\n";
  write_blocks outChannel blocks; (*writes the songs blocks to the output file*)
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