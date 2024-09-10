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

(* ---------------------instrument analysis------------------------------ *)
let analyse_instrument_test_bytes instrument state =
  let Instrument(Id(instrumentId), RegisterInstrument(b1, b2, b3, b4, _)) = instrument in
  let aux byteLabel byteValue state =
    if byteValue > 255 || byteValue < 0 then
      {state with messages = {state.messages with errors = ("byte "^byteLabel^" in instrument \""^instrumentId^"\" is not 8bit (should be a 0-255 value)")::state.messages.errors}}
    else
      state
  in
  aux "1" b1 (aux "2" b2 (aux "3" b3 (aux "" b4 state)))

let analyse_instrument_volume_first_value instrument state =
  let Instrument(Id(instrumentId), RegisterInstrument(_, _, _, _, volEnv)) = instrument in
  match volEnv with
  |[] -> {state with messages = {state.messages with errors = ("volume envelope in instrument \""^instrumentId^"\" is empty")::state.messages.errors}}
  |h::q -> if h = 0 then
    state
  else
    {state with messages = {state.messages with warnings = ("volume envelope in instrument \""^instrumentId^"\" starts with non-zero : you might want to reconfigure instrument base registers")::state.messages.warnings}}

let analyse_instrument_volume_range instrument state =
  let Instrument(Id(instrumentId), RegisterInstrument(_, _, _, _, volEnv)) = instrument in
  let rec aux volumeValues state =
    match volumeValues with
    |[] -> state
    |h::q -> if h > 15 || h < -16 then
      aux q {state with messages = {state.messages with errors = ("volume value in instrument \""^instrumentId^"\" is not 5bit signed (should be a -16~15 value)")::state.messages.errors}}
    else
      aux q state
  in
  aux volEnv state

let analyse_instruments_ammount state =
  (* there can't be more than 16 instruments *)
  if S.cardinal state.instrumentsIdentifiers > 16 then
    {state with messages = {state.messages with errors = ("too many instruments (only up to 16 can be defined)")::state.messages.errors}}
  else
    state

let analyse_instrument instrument state =
  (* test if 4 bytes are properly defined *)
  let state = analyse_instrument_test_bytes instrument state in
  (* test if volume envelope values are well defined (between -16 and 15) *)
  let state = analyse_instrument_volume_range instrument state in
  (* test if Volume envelope starts with 0 (could be an error) *)
  analyse_instrument_volume_first_value instrument state

let analyse_instruments globalAst state =
  let Ast(instruments, _, _) = globalAst in
  let rec aux instruments state =
    match instruments with
    |[] -> state
    |h::q -> aux q (analyse_instrument h state)
  in
  let state = analyse_instruments_ammount state in
  aux instruments state
(* ---------------------------------------------------------------------- *)

(* -------------------------------- songs analysis ----------------------------- *)
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

(* -------------------------------blocks Analysis---------------------------------- *)
(* Fonction anonyme de parcours de l'ast dans laquelle je peux envoyer plusieurs autres fonctions d'analyse *)
let ast_block_bfs astBlock appliedFunc state =
  let rec aux ast state =
  match ast with
  |Seq(a1, a2) -> let transitionState = aux a1 state in
                    aux a2 (appliedFunc (Seq(a1, a2)) transitionState)
  |Repeat(i, a) -> appliedFunc (Repeat(i, a)) (aux a state)
  |Transpose(i, a) -> appliedFunc (Transpose(i, a)) (aux a state)
  |WithVolume(vol, a) -> appliedFunc (WithVolume(vol, a)) (aux a state)
  |WithInstrument(inst, a) -> appliedFunc (WithInstrument(inst, a)) (aux a state)
  |Loop(a) -> appliedFunc (Loop(a)) (aux a state)
  |Call(a) -> appliedFunc (Call(a)) (aux a state)
  |Jump(a) -> appliedFunc (Jump(a)) (aux a state)
  |Note(n) -> appliedFunc (Note(n)) state
  |BlankNote -> appliedFunc (BlankNote) state
  |EmptyPulse -> appliedFunc (EmptyPulse) state
  |BlockId(i) -> appliedFunc (BlockId(i)) state
  in
  aux astBlock state

let analyse_block_verify_id_defined currentBlockId astNode state =
  match astNode with
  |BlockId(Id(id)) -> if not (S.mem id state.blockIdentifiers) then
    {state with messages = {state.messages with errors = ("Block id \""^id^"\" referenced in block \""^currentBlockId^"\" is not defined")::state.messages.errors}}
  else
    state
  |_-> state

let analyse_block_verify_call_and_jump_by_ref currentBlockId astNode state =
  match astNode with
  |Call(BlockId(a)) -> state
  |Jump(BlockId(a)) -> state
  |Call(_) -> {state with messages = {state.messages with errors = ("Call in block \""^currentBlockId^"\" is not done by block id")::state.messages.errors}}
  |Jump(_) -> {state with messages = {state.messages with errors = ("Jump in block \""^currentBlockId^"\" is not done by block id")::state.messages.errors}}
  |_-> state

let analyse_block_verify_range currentBlockId astNode state =
  match astNode with
  |Repeat(nb, _) -> if nb > 31 || nb < 0 then
    {state with messages = {state.messages with errors = ("bad repeat counter in block \""^currentBlockId^"\" (cannot exceed 31)")::state.messages.errors}}
  else
    state
  |WithVolume(vol, _) -> if vol > 15 || vol < 0 then
    {state with messages = {state.messages with errors = ("bad volume value in block \""^currentBlockId^"\" (cannot exceed 15)")::state.messages.errors}}
  else
    state
  |WithInstrument(inst, _) -> if inst > 15 || inst < 0 then
    {state with messages = {state.messages with errors = ("bad instrument index in blick \""^currentBlockId^"\" (cannot exceed 15)")::state.messages.errors}}
  else
    state
  |_-> state

let analyse_block block state =
  let Block(Id(id), astB) = block in
  (* test call pointers are defined *)
  let state = ast_block_bfs astB (analyse_block_verify_id_defined id) state in
  (* test call, and jumps are done only by reference *)
  let state = ast_block_bfs astB (analyse_block_verify_call_and_jump_by_ref id) state in
  (* test repeat counters, volume and instruments are in range *)
  let state = ast_block_bfs astB (analyse_block_verify_range id) state in
  state

let analyse_blocks globalAst state =
  let Ast(_, _, blocks) = globalAst in
  let rec aux blocks state =
    match blocks with
    |[] -> state
    |h::q -> aux q (analyse_block h state)
  in
  aux blocks state
(* -------------------------------------------------------------------------------- *)




(*---------------------------------------------------------------*)
(*---------------------- analyser functions ---------------------*)
(*---------------------------------------------------------------*)

let analyser_check globalAst =
  let messages = {errors = []; warnings = []} in
  let state = {messages = messages; instrumentsIdentifiers = S.empty; songIdentifiers = S.empty; blockIdentifiers = S.empty} in
  (* get identifiers and errors *)
  let state = get_identifiers globalAst state in
  (* Instruments analysis *)
  let state = analyse_instruments globalAst state in
  (* Song analysis *)
  let state = analyse_songs globalAst state in
  (* Blocks analysis *)
  let state = analyse_blocks globalAst state in
    (* display errors *)
  List.iter (Printf.eprintf "ERROR : %s\n") state.messages.errors;
    (* display warnings *)
  List.iter (Printf.printf "WARNING : %s\n") state.messages.warnings;
    (* Failwith if errors *)
  match state.messages.errors with
  |[] -> ()
  |_ -> failwith "There were ERRORS"