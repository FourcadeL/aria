open Audio
open BinaryTranslator

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_instruction outChannel instruction =
  let v = get_instruction_value instruction in
  let _ = Printf.fprintf outChannel "\tDB $%0*X ; %s \n" 2 v (instruction_string instruction) in
  match instruction with
  |CallBlock(id) -> Printf.fprintf outChannel "\tDB LOW(%s), HIGH(%s)\n" id id (*special cases of 2 bytes instructions*)
  |JumpBlock(id) -> Printf.fprintf outChannel "\tDB LOW(%s), HIGH(%s)\n" id id
  |_ -> ()


let rec write_instruction_list outChannel instruction_list =
  match instruction_list with
  |[] -> ()
  |h::q -> write_instruction outChannel h;
          write_instruction_list outChannel q;;

let write_block block outChannel sectionCounter =
  Printf.fprintf outChannel "\tSECTION \"songblock_%s\", ROMX\n" (string_of_int sectionCounter);
  let Block(id, instruction_list) = block in
  Printf.fprintf outChannel "%s:\n" id;
  write_instruction_list outChannel instruction_list;
  Printf.fprintf outChannel "\n";;


let write_instrument outChannel instr = 
  let rec aux_write_volumes_list l =
    match l with
      |[] -> failwith "error with volume envelope list : should not be empty"
      |[h] -> Printf.fprintf outChannel "$%0*X\n" 2 h (*ending sequence value*)
      |h::q -> Printf.fprintf outChannel "$%0*X, " 2 h; aux_write_volumes_list q (*modifier value*)
    in
  let Instrument(name, v1, v2, v3, v4, vlist) = instr in
  Printf.fprintf outChannel "%s:\n\tDB $%0*X, $%0*X, $%0*X, $%0*X\n" name 2 v1 2 v2 2 v3 2 v4;
  Printf.fprintf outChannel "\tDB ";
  aux_write_volumes_list vlist

let write_instrument_pointer outChannel instr =
  let Instrument(name, _, _, _, _, _) = instr in
  Printf.fprintf outChannel "\tDB LOW(%s), HIGH(%s)\n" name name

(*---------------------------------------------------------------*)
(*-----------------Local file writing functions------------------*)
(*---------------------------------------------------------------*)

let write_file_header outChannel =
  let infoHeader = "; this song file was generate using ARIA\n; for any informations please see :\n; https://github.com/FourcadeL/aria\n; \n; This aims to streamline music composition for the GB-engine tracker automaton\n; implemented by : https://github.com/FourcadeL/GB-Engine\n; \n; \n; ----------------\n; --- song file --\n; ----------------\n\n" in
  Printf.fprintf outChannel "%s" infoHeader;;


let write_instruments outChannel instrs =
  Printf.fprintf outChannel "\tSECTION \"instruments lookup\", ROMX, ALIGN[6]\n_instruments_lookup::\n";
  List.iter (write_instrument_pointer outChannel) instrs;
  Printf.fprintf outChannel "\n\tSECTION \"instruments\", ROMX\n";
  List.iter (write_instrument outChannel) instrs

let write_songs_channel_pointers outChannel songs =
  let rec aux song_list counter =
    match song_list with
    |[] -> ()
    |Song(name, Channel(id1), Channel(id2), Channel(id3), Channel(id4))::q ->
      Printf.fprintf outChannel "song_%s_%s::\n" (string_of_int counter) name;
      Printf.fprintf outChannel "\tDB LOW(%s), HIGH(%s), LOW(%s), HIGH(%s), LOW(%s), HIGH(%s), LOW(%s), HIGH(%s)\n" id1 id1 id2 id2 id3 id3 id4 id4;
      aux q (counter + 1);
  in
  Printf.fprintf outChannel "\tSECTION \"songs lookup\", ROMX\n";
  Printf.fprintf outChannel "songs_start::\n";
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
  (* let outChannel = open_out (fileName^".asm") in *)
  let outChannel = open_out fileName in
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

INSTRUMENT LOOKUP SECTION :
SECTION "instruments lookup", ROMX, ALIGN[6] (aligned as required by engine)
_instruments_lookup::
	DB LOW(inst_1), HIGH(inst_1)
	DB LOW(inst_2), HIGH(inst_2)
	DB LOW(inst_3), HIGH(inst_3)
	DB LOW(inst_4), HIGH(inst_4)

INSTRUMENT SECTION :
SECTION "instruments", ROMX (no need to align)
inst_1:
	DB $XX, $XX, $XX, $XX <- instruments values
	DB $XX, $XX, $XX, ...,  %10000000 (volume modifiers with terminaison)
inst_2:
	DB $XX, $XX, $XX, $XX
	DB $XX, $XX, $XX, ...,  %10000000 (volume modifiers with terminaison)
inst_3:
	DB $XX, $XX, $XX, $XX
	DB $XX, $XX, $XX, ...,  %10000000 (volume modifiers with terminaison)
inst_4:
	DB $XX, $XX, $XX, $XX
	DB $XX, $XX, $XX, ...,  %10000000 (volume modifiers with terminaison)


  SECTION "songs", ROMX (no need to align)
_song_name:
  DB HIGH(channel1_block_addr), HIGH(channel2_block_addr), HIGH(channel3_block_addr), HIGH(channel4_block_addr)
_song_name_2:
  DB HIGH(channel1_block_addr), HIGH(channel2_block_addr), HIGH(channel3_block_addr), HIGH(channel4_block_addr)
  .
  .
  .
  
  SECTION "songblock_1", ROMX
_block_name_1:
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb

  SECTION "songblock_2", ROMX
_block_name_2:
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
  DB %bbbbbbbb
*)