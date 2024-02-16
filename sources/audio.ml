(*octave type - denote an octave number*)
type octave =
|Oct of int

(*baseNote type - denote a note*)
type baseNote =
|C
|C#
|D
|D#
|E
|F
|F#
|G
|G#
|A
|A#
|B

(*not type - the full note characterisation*)
type note =
|Note of baseNote * octave

(*------------- Instrument type ---------------*)
type instrument =
Instrument of (int * int * int * int)

(*------------- Song and instructions type ---------------*)
type song =
Song of (Channel * Channel * Channel * Channel)

type channel =
Channel of (instruction list)

type instruction =
|PlayNote of note
|PlayEmpty
| %TODO



(*------------- Audio type ---------------*)
type audio =
Audio of instrument list * song list