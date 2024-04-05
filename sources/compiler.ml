(* this files contains methods and structures for translation of AST to audio structure *)
(* now only basic translation is done : no check nor memory optimisation are performed *)

open Audio
open Ast


type trackerState = {
  currentInstrument : int;
  currentVolume : int;
  currentTranspose : int;
}

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