(*this file handles Audio traduction, from parsed instruction lists
to actual automaton executable audio data*)

(*
Some rules to follow :
- Wait instructions are actually wait set : they must happend BEFORE the corresponding playnote
- Wait timers dont flush so two consecutive equal waittimes can be simplified
*)

open Audio

(*adds a wait(0) after each note in order to ensure null wait when no wait operator has been specified*)
let rec add_zero_waits instructionList=
  match instructionList with
  |[] -> []
  |PlayNote(n)::q -> PlayNote(n)::Wait(0)::add_zero_waits q
  |PlayEmpty::q -> PlayEmpty::Wait(0)::add_zero_waits q
  |h::q -> h::add_zero_waits q

(*when multiple waits are not separated by a note,
   merges them into one wait sum*)
let rec merge_waits instructionList =
  let rec aux_construct tailingInstructions counter outbuffer =
    match tailingInstructions with
    |[] -> Wait(counter)::outbuffer
    |PlayNote(_)::q -> Wait(counter)::outbuffer@tailingInstructions
    |PlayEmpty::q -> Wait(counter)::outbuffer@tailingInstructions
    |Wait(n)::q -> aux_construct q (counter + n) outbuffer
    |h::q -> aux_construct q counter (outbuffer@[h])
  in
  match instructionList with
  |[] -> []
  |Wait(n)::q -> (match (aux_construct q n []) with
                  | [] -> failwith "runtime error"
                  | w::t -> w::merge_waits t)
  |h::q -> h::merge_waits q



(*passes wait instructions before their associated notes
   for counter setting style of automaton
   assumes waits are already merged and zero added
   (i.e : there is no consecutive notes without at least a Wait(0) instruction between them)
   Example : X X Note Wait() X Note X X Wait() X Note Wait
   becomes
   X X Wait() Note X Wait() Note X X X Wait() Note *)
let rec pass_wait_instruction instructionList =
  let rec aux_get_next_wait iList =
    match iList with
    |[] -> failwith "unexpected empty list"
    |PlayNote(_)::q -> failwith "unexpected next note"
    |PlayEmpty::q -> failwith "unexpected next note"
    |Wait(x)::q -> Wait(x)
    |h::q -> aux_get_next_wait q
  in
  match instructionList with
  |[] -> []
  |PlayNote(n)::q -> (aux_get_next_wait q)::PlayNote(n)::(pass_wait_instruction q)
  |PlayEmpty::q -> (aux_get_next_wait q)::PlayEmpty::(pass_wait_instruction q)
  |Wait(_)::q -> pass_wait_instruction q
  |h::q -> h::pass_wait_instruction q


(*When two consecutive wait at execution have the same value
   remove the unnecessary ones
   example here (x are instructions)
   
   X Wait(3) X X X Wait(3) X Wait(2) X X Wait(1) X Wait(1) X
    |
   X Wait(3) X X X X Wait(2) X X Wait(1) X X
   
   keeps waits when passing through call or return tracker set since
   wait state becomes unsure*)
let remove_duplicate_waits instructionList =
  let rec aux tailingList previousWaitValue =
    match tailingList with
    |[] -> []
    |Wait(n)::q -> if (previousWaitValue = n) then (aux q previousWaitValue) else Wait(n)::(aux q n)
    |CallBlock(x)::q -> CallBlock(x)::(aux q (-1))
    |SetReturnTrack::q -> SetReturnTrack::(aux q (-1)) (*in such cases we might resume with different wait values*)
    |h::q -> h::(aux q previousWaitValue)
  in
  aux instructionList (-1)


(*Remove superfluous instrument assigment that can arise from macro usage
Example : X X X InstrumentSet(3) InstrumentSet(5) X X X
(here instrument 3 can be removed)*)
let remove_superfluous_instruments_assignments instructionList =
  let rec find_note_before_next_instrument_assign tailingList =
    match tailingList with
    |PlayNote(_)::q -> true
    |InstrumentSet(_)::q -> false
    |[] -> false
    |CallBlock(_)::q -> true (*callBlocks can contain notes*)
    |JumpBlock(_)::q -> true (*jumpBlocks can contain notes*)
    |_::q -> find_note_before_next_instrument_assign q
  in
  let rec aux tailingList previousInstrumentValue =
    match tailingList with
    |[] -> []
    |InstrumentSet(i)::q -> if (previousInstrumentValue = i || not (find_note_before_next_instrument_assign q)) then
                              (aux q previousInstrumentValue)
                            else
                              InstrumentSet(i)::(aux q i)
    |CallBlock(x)::q -> CallBlock(x)::(aux q (-1))
    |SetReturnTrack::q -> SetReturnTrack::(aux q (-1)) (*can resume with different instrument values*)
    |h::q -> h::(aux q previousInstrumentValue)
  in
  aux instructionList (-1)

let remove_superfluous_volume_assignments instructionList =
  let rec find_note_before_next_volume_assign tailingList =
    match tailingList with
    |PlayNote(_)::q -> true
    |VolumeSet(_)::q -> false
    |[] -> false
    |CallBlock(_)::q -> true (*callBlocks can contain notes*)
    |JumpBlock(_)::q -> true (*jumpBlocks can contain notes*)
    |_::q -> find_note_before_next_volume_assign q
  in
  let rec aux tailingList previousVolumeValue =
    match tailingList with
    |[] -> []
    |VolumeSet(v)::q -> if (previousVolumeValue = v || not (find_note_before_next_volume_assign q)) then
                          (aux q previousVolumeValue)
                        else
                          VolumeSet(v)::(aux q v)
    |CallBlock(x)::q -> CallBlock(x)::(aux q (-1))
    |SetReturnTrack::q -> SetReturnTrack::(aux q (-1)) (*can resume with different volume values*)
    |h::q -> h::(aux q previousVolumeValue)
  in
  aux instructionList (-1)


let compress_block block =
  let Block(id, il) = block in
  let p1 = add_zero_waits il in (*adds missing Wait(0)*)
  let p2 = merge_waits p1 in (*merge multiple consecutive waits*)
  let p3 = pass_wait_instruction p2 in (*pass waits for automaton execution style*)
  let p4 = remove_duplicate_waits p3 in (*removes superfluous sets to same wait value*)
  let p5 = remove_superfluous_instruments_assignments p4 in (*removes superfluous instruments sets*)
  let li = remove_superfluous_volume_assignments p5 in (*removes superfluous volume sets*)
  Block(id, li)


let compress_audio audio =
  let Audio(inst, songs, blocks) = audio in
  Audio(inst, songs, List.map (compress_block) blocks)
