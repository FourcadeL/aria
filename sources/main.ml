open Audio
open RgbdsFileGenerator
open Checker
open Compress



(* main actions *)

(* small parsing (i.e. no step by step error checking) *)

let _ = Printf.printf "Parsing of input file ...\n"
let lexbuf = Lexing.from_channel stdin

let audioParsed = Parser.start (Lexer.read) lexbuf;;

(* quick validity checks *)
Printf.printf "Checking validity ...\n";;
let Audio(instruments, songs, blocks) = audioParsed in
check_songs_pointers songs blocks;
let testBlockSizes = check_blocks_size blocks in
if not testBlockSizes then failwith "there were errors in file";

(*compression pass (debug)*)
let Audio(_, _, bloc::q) = audioParsed in
Audio.block_display bloc;
let Block(_, instrs) = bloc in
let b = merge_waits instrs in
let bb = Block(Id("zzz"), b) in
Audio.block_display bb;

(* output rgbds file *)

Printf.printf "Outputin rgbds file ...\n";;
output_rgbds_file audioParsed "test"