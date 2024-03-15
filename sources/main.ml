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

(*compression pass*)
let audioCompressed = compress_audio audioParsed in

(* output rgbds file *)

Printf.printf "Outputin rgbds file ...\n";
output_rgbds_file audioCompressed "test"