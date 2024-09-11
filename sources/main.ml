open Audio
open Ast
open RgbdsFileGenerator
open Checker
open Compiler
open Compress
open Transform
open Analyser




(* -------------------------------------------------------------------------------- *)
(* ------------------------- Main actions functions ------------------------------- *)
(* -------------------------------------------------------------------------------- *)



(* main actions *)

(* arguments parssing *)
let program_name = "aria"
let program_version = "0.3"
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

    (*AST analysis*)
    analyser_check parsed;

    (*AST transformation pass*)
    (*---------- testing------------*)
    (* disp_ast parsed; *)
    debug parsed;
    let parsed = transform_ast parsed in
    debug parsed;
    (* ---------------------------- *)

    (* AST compilation into Audio *)
    (* ------------------------------- *)
    let compiledAudio = compile_globalAst_to_audio parsed in
    (* ------------------------------- *)

    (*compression pass without checks*)
    let Audio(instruments, songs, blocks) = compress_audio compiledAudio in
    if not (check_blocks_size blocks) then failwith "there were errors in file";

    output_rgbds_file (Audio(instruments, songs, blocks)) !output_file;

    print_endline "Done"
  
  end