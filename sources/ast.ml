(*Abstract Syntaxic Tree for blocks*)
open Audio

(*identifier used for declarations*)
type identifier =
|Id of string

(*Instruments*)
type astVolumeEnvelope = int list

type astInstrument =
|RegisterInstrument of (int * int * int * int * int list)

type structInstrument = Instrument of (identifier * astInstrument)

(*Songs*)
type astSong = PointersSong of (identifier * identifier * identifier * identifier)

type structSong = Song of (identifier * astSong)

(*Blocks*)
type astBlock =
    (*Tree nodes*)
|Seq of astBlock * astBlock
|Repeat of int * astBlock
|Transpose of int * astBlock
|WithVolume of int * astBlock
|WithInstrument of int * astBlock
|Loop of astBlock
|Call of astBlock
|Jump of astBlock
    (*Tree leafs*)
|Note of note
|BlankNote
|EmptyPulse
|BlockId of identifier

type structBlock = Block of (identifier * astBlock)

type globalAst = Ast of (structInstrument list) * (structSong list) * (structBlock list)


(*---------------------------------------------------------------*)
(*--------------------   display function   ---------------------*)
(*---------------------------------------------------------------*)

let disp_ast a =
  let Ast(instruments, songs, blocks) = a in
  Printf.printf "not yet implemented";