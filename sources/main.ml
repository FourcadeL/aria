open Audio
open Ast
open RgbdsFileGenerator
open Checker
open Compiler
open Compress






(* -------------------------------------------------------------------------------- *)
(* ------------------------- Main actions functions ------------------------------- *)
(* -------------------------------------------------------------------------------- *)
(* unclear for compilation
   some functions are just passthrough (instruments and songs) 
   but will have to be changed in the future*)

let instrument_list_from_ast globalAst =
  let Ast(instruments, _, _) = globalAst in
  let rec aux currInstrus =
    match currInstrus with
    |[] -> []
    |Instrument(Id(id), RegisterInstrument(r1, r2, r3, r4))::q -> Audio.Instrument(id, r1, r2, r3, r4)::aux q
  in
  aux instruments

let song_list_from_ast globalAst =
  let Ast(_, songs, _) = globalAst in
  let rec aux currSongs =
    match currSongs with
    |[] -> []
    |Song(Id(id), PointersSong(Id(idb1), Id(idb2), Id(idb3), Id(idb4)))::q ->
      Audio.Song(id, Channel(idb1), Channel(idb2), Channel(idb3), Channel(idb4))::aux q
  in
  aux songs

let block_list_from_ast globalAst =
  let Ast(_, _, blocks) = globalAst in
  let rec aux currBlocks =
    match currBlocks with
    |[] -> []
    |b::q -> (astStructBlock_to_audioBlock b)::aux q
  in
  aux (blocks)

(* main actions *)

(* small parsing (i.e. no step by step error checking) *)

let _ = Printf.printf "Parsing of input file ...\n";;
let lexbuf = Lexing.from_channel stdin;;
let parsed = Parser.start (Lexer.read) lexbuf;;

(* translation (still weak) of Ast into audio structures*)
let instruments = instrument_list_from_ast parsed;;
let songs = song_list_from_ast parsed;;
let blocks = block_list_from_ast parsed;;


(* quick validity checks *)
Printf.printf "Checking validity ...\n";;
check_songs_pointers songs blocks;

(*compression pass*)
let audioCompressed = compress_audio (Audio(instruments, songs, blocks)) in
let Audio(instruments, songs, blocks) = audioCompressed in
let testBlockSizes = check_blocks_size blocks in
if not testBlockSizes then failwith "there were errors in file";

(* output rgbds file *)

Printf.printf "Outputin rgbds file ...\n";
output_rgbds_file audioCompressed "test"