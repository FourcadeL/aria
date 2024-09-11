(* this files contains methods and structures for translation of AST to audio structure *)
(* now only basic translation is done : no check nor memory optimisation are performed *)

open Audio
open Ast
open BinaryTranslator


type trackerState = {
  currentInstrument : int;
  currentVolume : int;
  currentTranspose : int;
}

(*---------------------------------------------------------------*)
(*---------------------- internal functions ---------------------*)
(*---------------------------------------------------------------*)

(* the function isn't terminal recursive : to be optimized
   - Current transpose is done by shifting seen notes BUT NOTE FUNCITON CALLS
   - Loop is done only with a complete return of tracker, not partial
   - since no preliminary optimization is done call and jump can't be called on ast other than blockID*)
let astStructBlock_to_audioBlock astStructBlock =
  let rec auxTranslate buffer currAst currState =
    match currAst with
    |Seq(a1, a2) -> auxTranslate (auxTranslate buffer a1 currState) a2 currState
    |Repeat(i, a) -> (auxTranslate (buffer@[RepeatCounterSet(i-1); SetReturnTrack]) a currState)@[ConditionnalReturnTrack]
    |Transpose(i, a) -> let newState = {currState with currentTranspose = i} in
                          auxTranslate buffer a newState
    |WithVolume(v, a) -> let newState = {currState with currentVolume = v} in
                          (auxTranslate (buffer@[VolumeSet(v)]) a newState)@[VolumeSet(currState.currentVolume)]
    |WithInstrument(i, a) -> let newState = {currState with currentInstrument = i} in
                          (auxTranslate (buffer@[InstrumentSet(i)]) a newState)@[InstrumentSet(currState.currentInstrument)]
    |Loop(a) -> (auxTranslate buffer a currState)@[GlobalReturnTrack]
    |Call(BlockId(Id(s))) -> buffer@[CallBlock(s)]
    |Jump(BlockId(Id(s))) -> buffer@[JumpBlock(s)]
    |Note(n) -> buffer@[PlayNote(transpose_note n currState.currentTranspose)]
    |BlankNote -> buffer@[PlayEmpty]
    |EmptyPulse -> buffer@[Wait(1)]
    |_ -> failwith "undefined behaviour : get to work"
  in
  let Block(Id(id), ast) = astStructBlock in
  let initialState = {currentInstrument = 0; currentVolume = 10; currentTranspose = 0} in
  Audio.Block(id, (auxTranslate [] ast initialState)@[EndBlock])



(* Translate an ast instrument structure into an audio structure
  - keeps register values as-is
  - compile volume enveloppe instruction to encode repetition
    -reminder : %10000000 -> sequence terminaison string (added as last value of the list)
                %000xxxxx -> modifier value 5 bit modifier for volume (signed value)
                %01xxxxxx -> wait instruction : will wait xxxxxx update tick before next volume update (added as 8bit value, this function compile repetition instructions)
                *)
let astStructInstrument_to_audioInstrument astStructInstrument =
  let rec aux_volumeEnvelopeList_to_volumeInstructionList volList lastValue lastValueCounter =
    match volList with
    |[] -> [0b10000000]
    |h::q when h==lastValue -> aux_volumeEnvelopeList_to_volumeInstructionList q lastValue (lastValueCounter + 1)
    |h::q when lastValueCounter > 0 -> (0b01000000 + lastValueCounter)::(get_audio_5bitsigned_modifier_value h)::(aux_volumeEnvelopeList_to_volumeInstructionList q h 0)
    |h::q -> (get_audio_5bitsigned_modifier_value h)::(aux_volumeEnvelopeList_to_volumeInstructionList q h 0)
  in
  let Instrument(Id(id), RegisterInstrument(r1, r2, r3, r4, vl)) = astStructInstrument in
  Audio.Instrument(id, r1, r2, r3, r4, aux_volumeEnvelopeList_to_volumeInstructionList vl 99 0)


let instrument_list_from_ast globalAst =
  let Ast(instruments, _, _) = globalAst in
  let rec aux currInstrus =
    match currInstrus with
    |[] -> []
    |h::q -> (astStructInstrument_to_audioInstrument h)::aux q
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

  
(*---------------------------------------------------------------*)
(*---------------------- compiler functions ---------------------*)
(*---------------------------------------------------------------*)

let compile_globalAst_to_audio globalAst =
  let instruments = instrument_list_from_ast globalAst in
  let songs = song_list_from_ast globalAst in
  let blocks = block_list_from_ast globalAst in
  Audio(instruments, songs, blocks)
