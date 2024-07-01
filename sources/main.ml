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
    |Instrument(Id(id), RegisterInstrument(r1, r2, r3, r4, volVals))::q -> Audio.Instrument(id, r1, r2, r3, r4, volVals)::aux q
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

(* arguments parssing *)
let program_name = "aria"
let program_version = "0.2"
let usage_msg = Printf.sprintf "%s - version %s\n usage : [-verbose] -f <file1> [-o <output>]" program_name program_version
let verbose = ref false
let input_file = ref ""
let output_file = ref ""

let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
  ("-f", Arg.Set_string input_file, "Set input file name");
  ("-o", Arg.Set_string output_file, "Set output file name")]

(** parse_args function *)
let parse_cmdline =
  begin
    Arg.parse speclist print_endline usage_msg;
    try
      if !input_file = "" then
        raise (Arg.Bad ("missing argument: no input file name given"));
      if !output_file = "" then
        output_file := "out";
    with
    | Arg.Bad msg ->
       Printf.eprintf "%s: %s\n" Sys.argv.(0) msg; Printf.eprintf "%s\n" usage_msg; exit 1;
  end



(* Main functionality here *)
let () =
  begin
    (* parsing of arguments *)
    parse_cmdline;
    print_endline "Running the rest of the program... with:";
    Printf.printf " verbosity = %b\n" !verbose;
    Printf.printf " input_file = %s\n" !input_file;
    Printf.printf " output_file = %s\n" !output_file;

    (*AST creation from file*)
    let input_channel = open_in !input_file in
    let lexbuf = Lexing.from_channel input_channel in
    let parsed = Parser.start (Lexer.read) lexbuf in

    (*weak translation of single ast into audio structures*)
    (*TO BE CHANGED*)
    let instruments = instrument_list_from_ast parsed and
    songs = song_list_from_ast parsed  and
    blocks = block_list_from_ast parsed in

    (*compression pass without checks*)
    let Audio(instruments, songs, blocks) = compress_audio (Audio(instruments, songs, blocks)) in
    if not (check_blocks_size blocks) then failwith "there were errors in file";

    output_rgbds_file (Audio(instruments, songs, blocks)) !output_file;

    print_endline "Done"
  end