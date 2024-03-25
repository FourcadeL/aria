(*Abstract Syntaxic Tree for blocks*)

type astBlock =
(*Tree nodes*)
|Seq of astBlock * astBlock
|Repeat of int * astBlock
|Transpose of int * astBlock
|WithVolume of int * astBlock
|WithInstrument of int * astBlock
|Call of astBlock
|Jump of astBlock
(*Tree leafs*)
|Note of note
|BlankNote
|EmptyPulse
|Id of string