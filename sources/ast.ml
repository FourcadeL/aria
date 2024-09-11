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
|WithInstrument of identifier * astBlock
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

let disp_ast_song s =
    let Song(Id(id), PointersSong(Id(ch1), Id(ch2), Id(ch3), Id(ch4))) = s in
    Printf.printf "Song \"%s\" :\n\t|CH1 : %s\n\t|CH2 : %s\n\t|CH3 : %s\n\t|CH4 : %s\n\n" id ch1 ch2 ch3 ch4

let disp_ast_block b =
    let Block(Id(id), astB) = b in
    let rec aux_instruction_display treeBuff astI =
        match astI with
        |Seq(a1, a2) -> Printf.printf "%s+--+\n" treeBuff; aux_instruction_display (treeBuff^"|  ") a2; aux_instruction_display treeBuff a1
        |Repeat(nb, a) -> Printf.printf "%sRepeat(%i)\n" treeBuff nb; aux_instruction_display (treeBuff^"   ") a
        |Transpose(nb, a) -> Printf.printf "%sTranspose(%i)\n" treeBuff nb; aux_instruction_display (treeBuff^"   ") a
        |WithVolume(vol, a) -> Printf.printf "%sWithVolume(%i)\n" treeBuff vol; aux_instruction_display (treeBuff^"   ") a
        |WithInstrument(Id(inst), a) -> Printf.printf "%sWithInstrument(%s)\n" treeBuff inst; aux_instruction_display (treeBuff^"   ") a
        |Loop(a) -> Printf.printf "%sLoop\n" treeBuff; aux_instruction_display (treeBuff^"   ") a
        |Call(a) -> Printf.printf "%sCall\n" treeBuff; aux_instruction_display (treeBuff^"   ") a
        |Jump(a) -> Printf.printf "%sJump\n" treeBuff; aux_instruction_display (treeBuff^"   ") a
        |Note(n) -> Printf.printf "%s%s\n" treeBuff (note_string n)
        |BlankNote -> Printf.printf "%s_\n" treeBuff
        |EmptyPulse -> Printf.printf "%s.\n" treeBuff
        |BlockId(Id(id)) -> Printf.printf "%s[%s]\n" treeBuff id
        |_ -> Printf.printf "%sError\n" treeBuff;
    in
    Printf.printf "Block \"%s\" :\n" id;
    aux_instruction_display "" astB

let disp_ast a =
  let Ast(instruments, songs, blocks) = a in
  Printf.printf "instrument display : not yet implemented\n";
  List.iter disp_ast_song songs;
  List.iter disp_ast_block blocks




