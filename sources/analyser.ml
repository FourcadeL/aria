open Ast

module S = Set.Make(String);;

(* Bundle structure for Error and Warning messages
Messages are just stored as strings*)
type messagesBundle = {
  errors : string list; (*error messages*)
  warnings : string list; (*warning messages*)
}

type currentAnalysisState = {
  messages : messagesBundle; (*Current messages of the analysis*)
  instrumentsIdentifiers : S.t; (*Currently defined instrument identifiers*)
  songIdentifiers : S.t; (*Currently defined song identifiers*)
  blockIdentifiers : S.t; (*Currently defined block identifiers*)
}


(* ------------------------get identifiers----------------------------- *)
let get_block_identifiers globalAst state =
  let Ast(_, _, blocks) = globalAst in
  let rec aux blockTailingList currState =
    let messages = currState.messages and blockIdentifiers=currState.blockIdentifiers in
    match blockTailingList with
    |[] -> currState
    |Block(Id(id), _)::q -> if S.mem id blockIdentifiers then
                            aux q {currState with messages = {messages with errors = ("block id \""^id^"\" defined multiple times")::messages.errors}}
                            else
                              aux q {currState with blockIdentifiers = S.add id blockIdentifiers}
  in
  aux blocks state

let get_song_identifiers globalAst state =
  let Ast(_, songs, _) = globalAst in
  let rec aux songTailingList currState =
    let messages = currState.messages and songIdentifiers = currState.songIdentifiers in
    match songTailingList with
    |[] -> currState
    |Song(Id(id), _)::q -> if S.mem id songIdentifiers then
                            aux q {currState with messages = {messages with errors = ("song id \""^id^"\" defined multiple times")::messages.errors}}
                          else
                            aux q {currState with songIdentifiers = S.add id songIdentifiers}
  in
  aux songs state

let get_instrument_identifiers globalAst state =
  let Ast(instruments, songs, blocks) = globalAst in
  let rec aux instrTailingList currState =
    let messages = currState.messages and instrIdentifiers = currState.instrumentsIdentifiers in
    match instrTailingList with
    |[] -> currState
    |Instrument(Id(id), _)::q -> if S.mem id instrIdentifiers then
                                  aux q {currState with messages = {messages with errors = ("instrument id \""^id^"\" defined multiple times")::messages.errors}}
                                else
                                  aux q {currState with instrumentsIdentifiers = S.add id instrIdentifiers}
  in
  aux instruments state

let get_identifiers globalAst state =
  let p1 = get_instrument_identifiers globalAst state in
  let p2 = get_song_identifiers globalAst p1 in
  get_block_identifiers globalAst p2
(* ----------------------------------------------------- *)

(* --------------------------------analyse songs----------------------------- *)
let analyse_song song state =
  let Song(Id(songId), PointersSong(Id(ch1), Id(ch2), Id(ch3), Id(ch4))) = song in
  let aux channelId state =
    if not (S.mem channelId state.blockIdentifiers) then
      {state with messages = {state.messages with errors = ("block id \""^channelId^"\" referenced in \""^songId^"\" is undefined")::state.messages.errors}}
    else
      state
  in
  aux ch1 (aux ch2 (aux ch3 (aux ch4 state)))

let analyse_songs globalAst state =
  let Ast(_, songs, _) = globalAst in
  let rec aux songs state =
    match songs with
    |[] -> state
    |h::q -> aux q (analyse_song h state)
  in
  aux songs state
(* -------------------------------------------------------------------------- *)


(*---------------------------------------------------------------*)
(*---------------------- analyser functions ---------------------*)
(*---------------------------------------------------------------*)

let analyser_check globalAst =
  let messages = {errors = []; warnings = []} in
  let state = {messages = messages; instrumentsIdentifiers = S.empty; songIdentifiers = S.empty; blockIdentifiers = S.empty} in
  (* get identifiers and errors *)
  let state = get_identifiers globalAst state in
  (* Song analysis *)
  let state = analyse_songs globalAst state in
  (* Blocks analysis *)
  (* final display of analysis (and fail if errors) *)
    (* display errors *)
  List.iter (Printf.eprintf "ERROR : %s\n") state.messages.errors;
    (* display warnings *)
  List.iter (Printf.printf "WARNING : %s\n") state.messages.warnings;
    (* Failwith if errors *)
  match state.messages.errors with
  |[] -> ()
  |_ -> failwith "There were ERRORS"