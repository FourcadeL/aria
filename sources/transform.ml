(*Post parsing and Pre compilation
transofrmation of ast for anonymous replacement*)
open Ast

(*Counter struct for number of call and number of anonymous places*)
type analyseCounter = {
  callCounter : int; (*number af call to that block*)
  jumpCounter : int; (*number of jumps to that block*)
  anonymousCounter : int; (*number of anonymous calls to that block*)
  songReferenceCounter : int; (*number of reference to that block in songs*)
  blockSize : int; (*size score of that block*)
}



(*-----------------*)
(*return size of ast block
-> heuristic : give a "size score" to the ast instruction*)
let getAstBlockSizeScore astBlock =
  let Block(_, astB) = astBlock in
  let rec aux currAst counter =
    match currAst with
    |Seq(a1, a2) -> aux a2 (aux a1 counter)
    |Repeat(_, a) -> aux a counter
    |Transpose(_, a) -> aux a counter
    |WithVolume(_, a) -> aux a counter
    |WithInstrument(_, a) -> aux a counter
    |Loop(a) -> aux a counter
    |Call(_) -> counter + 2
    |Jump(_) -> counter + 2
    |Note(_) -> counter + 1
    |BlankNote -> counter + 1
    |EmptyPulse -> counter
    |_ -> counter + 255 (*heuristic -> cant transform a block containing anonymous instruction of BlockId(_)*)
  in
  aux astB 0


(*-----------------*)



(*Take a struct block and the global ast
returns the counter associated to the block*)
let analyse_block block globalAst =
  let Block(Id(id), _)  = block in
  let aux_count_in_block idToTest accCounter block =
    let Block(_, astB) = block in
    let rec inter_aux_iterate_ast idToTestInner ast counter =
      match ast with
      |Seq(a1, a2) -> let c1 = inter_aux_iterate_ast id a1 counter in inter_aux_iterate_ast id a2 c1
      |Call(BlockId(Id(s))) when s = idToTestInner -> {counter with callCounter = counter.callCounter + 1}
      |Jump(BlockId(Id(s))) when s = idToTestInner -> {counter with jumpCounter = counter.jumpCounter + 1}
      |BlockId(Id(s)) when s = idToTestInner -> {counter with anonymousCounter = counter.anonymousCounter + 1}
      |Repeat(_, a) -> inter_aux_iterate_ast idToTestInner a counter
      |Transpose(_, a) -> inter_aux_iterate_ast idToTestInner a counter
      |WithVolume(_, a) -> inter_aux_iterate_ast idToTestInner a counter
      |WithInstrument(_, a) -> inter_aux_iterate_ast idToTestInner a counter
      |Loop(a) -> inter_aux_iterate_ast idToTestInner a counter
      |_ -> counter
    in
  inter_aux_iterate_ast idToTest astB accCounter
  in
  let aux_count_in_song idToTest accCounter song =
    let Song(_, PointersSong(Id(b1), Id(b2), Id(b3), Id(b4))) = song in
    let localCount = ref 0 in
    if b1 = idToTest then localCount := !localCount + 1;
    if b2 = idToTest then localCount := !localCount + 1;
    if b3 = idToTest then localCount := !localCount + 1;
    if b4 = idToTest then localCount := !localCount + 1;
    {accCounter with songReferenceCounter = accCounter.songReferenceCounter + !localCount}
  in
  let baseCounter = {callCounter = 0; jumpCounter = 0; anonymousCounter = 0; songReferenceCounter = 0; blockSize = getAstBlockSizeScore block} in
  let Ast(_, songList, blocksList) = globalAst in
  let counterWithSongs = List.fold_left (aux_count_in_song id) baseCounter songList in
  List.fold_left (aux_count_in_block id) counterWithSongs blocksList


(* -------------------- Anonymous and call transformation functions ------------------- *)

(*takes a block and a global Ast
returns the global ast where the block has been removed and every call or anonymous call to the block
has been replaced by its content*)
let anonymous_transform block globalAst =
  let Ast(instruments, songs, blocks) = globalAst and
  Block(Id(currId), astB) = block in
  Printf.printf "Anonymous transform of \"%s\"\n" currId;
  let rec aux_transform_ast tmpAst =
    match tmpAst with
    |Seq(a1, a2) -> Seq(aux_transform_ast a1, aux_transform_ast a2)
    |Repeat(nb, a) -> Repeat(nb, aux_transform_ast a)
    |Transpose(nb, a) -> Transpose(nb, aux_transform_ast a)
    |WithVolume(vol, a) -> WithVolume(vol, aux_transform_ast a)
    |WithInstrument(inst, a) -> WithInstrument(inst, aux_transform_ast a)
    |Loop(a) -> Loop(aux_transform_ast a)
    |Call(BlockId(Id(id))) -> if id = currId then astB else Call(BlockId(Id(id)))
    |Jump(BlockId(Id(id))) -> Jump(BlockId(Id(id)))
    |BlockId(Id(id)) -> if id = currId then astB else BlockId(Id(id))
    |other -> other
  in
  let rec update_block_list blockList =
    match blockList with
    |[] -> []
    |h::q -> let Block(Id(id), tmpAst) = h in
              if id=currId then
                update_block_list q
              else
                Block(Id(id), aux_transform_ast tmpAst)::update_block_list q
  in
  Ast(instruments, songs, update_block_list blocks)

  (*takes a block and a global Ast
  returns the global ast where every anonymous reference to the block
  has been replaced by a call*)
let call_transform block globalAst =
  let Ast(instruments, songs, blocks) = globalAst and
  Block(Id(currId), _) = block in
  Printf.printf "Call transform of \"%s\"\n" currId;
  let rec aux_transform_ast tmpAst =
    match tmpAst with
    |Seq(a1, a2) -> Seq(aux_transform_ast a1, aux_transform_ast a2)
    |Repeat(nb, a) -> Repeat(nb, aux_transform_ast a)
    |Transpose(nb, a) -> Transpose(nb, aux_transform_ast a)
    |WithVolume(vol, a) -> WithVolume(vol, aux_transform_ast a)
    |WithInstrument(inst, a) -> WithInstrument(inst, aux_transform_ast a)
    |Loop(a) -> Loop(aux_transform_ast a)
    |Call(BlockId(Id(id))) -> Call(BlockId(Id(id)))
    |Jump(BlockId(Id(id))) -> Jump(BlockId(Id(id)))
    |BlockId(Id(id)) -> if id = currId then Call(BlockId(Id(currId))) else BlockId(Id(id))
    |other -> other
  in
  let rec update_block_list blockList =
    match blockList with
    |[] -> []
    |h::q -> let Block(Id(id), tmpAst) = h in Block(Id(id), aux_transform_ast tmpAst)::update_block_list q
  in
  Ast(instruments, songs, update_block_list blocks)

(* ------------------------------------------------------------------------- *)

(* ----------------------- Recursive Repeat transformation Functions ----------- *)

(*Takes a block
Returns the list containing block + every new blocks spawned from recursive transformation*)
let create_recursive_repeat_blocks block =
  let Block(Id(blockId), astB) = block in
  let resultList = ref [] in
  let rec aux_construct_truncated_ast currAst deepness tailingLabel =
    match currAst with
    |Seq(a1, a2) -> Seq(aux_construct_truncated_ast a1 deepness (tailingLabel^"l"), aux_construct_truncated_ast a2 deepness tailingLabel)
    |Transpose(i, a) -> Transpose(i, aux_construct_truncated_ast a deepness tailingLabel)
    |WithVolume(v, a) -> WithVolume(v, aux_construct_truncated_ast a deepness tailingLabel)
    |WithInstrument(id, a) -> WithInstrument(id, aux_construct_truncated_ast a deepness tailingLabel)
    |Loop(a) -> Loop(aux_construct_truncated_ast a deepness tailingLabel)
    |Call(a) -> Call(aux_construct_truncated_ast a deepness tailingLabel)
    |Jump(a) -> Jump(aux_construct_truncated_ast a deepness tailingLabel)
    |Repeat(i, a) -> if deepness < 1 then
      Repeat(i, aux_construct_truncated_ast a (deepness+1) tailingLabel)
    else
      let newLabel = tailingLabel^(string_of_int deepness) in
      let subAstInBlock = Repeat(i, aux_construct_truncated_ast a (deepness+1) tailingLabel) in
      resultList := Block(Id(newLabel), subAstInBlock)::!resultList;
      Call(BlockId(Id(newLabel)))
    |other -> other
  in
  let newAstForBlock = aux_construct_truncated_ast astB 0 (blockId^"_reprec") in
  Block(Id(blockId), newAstForBlock)::(!resultList)

(* ----------------------------------------------------------------------------- *)

(*---------------------------------------------------------------*)
(*--------------------      functions       ---------------------*)
(*---------------------------------------------------------------*)

(*Takes an ast
analyse usage of jump, call and anonymous call of block$
replaces or deletes calls and blocks according to the rules
block :
- never called or referenced : kept untotched
- jumped to or heuristic size too large : kept untoutched
- heuristic block size < 10 : anonymous transform -> blocked deleted and every call and reference are replaced by block content
- called or referenced more than 4 times : call tranformed -> every reference is transformed into a call
- called less than 3 times : anonymous transform *)
let transform_anonymous_and_call_ast globalAst =
  let Ast(instruments, songs, blocks) = globalAst in
  let rec aux_iterate_on_blocks currentGlobalAst blocks =
    match blocks with
    |[] -> currentGlobalAst
    |h::q -> let analyseUsage = analyse_block h globalAst in
              if (analyseUsage.callCounter=0 && analyseUsage.anonymousCounter=0) then
                aux_iterate_on_blocks currentGlobalAst q (*never calles, kept untoutched*)
              else
                if analyseUsage.jumpCounter > 0 || analyseUsage.blockSize > 200 then
                  aux_iterate_on_blocks (call_transform h currentGlobalAst) q (*jump or too large, must keep block*)
                else
                  if analyseUsage.blockSize < 10 then
                    aux_iterate_on_blocks (anonymous_transform h currentGlobalAst) q (*small size, transform for anonymous*)
                  else
                    if analyseUsage.callCounter + analyseUsage.anonymousCounter > 3 then
                      aux_iterate_on_blocks (call_transform h currentGlobalAst) q (*largely called, transform for call*)
                    else
                      aux_iterate_on_blocks (anonymous_transform h currentGlobalAst) q (*sparsely called, transform for anonymous*)
  in
  aux_iterate_on_blocks globalAst blocks



(*Takes an ast
transforme recursively nested repeats into calls to newly created blocks
to avoid conflics in multiply recursive structures
Example : X X Repeat(i) {X X Repeat(j) {X X}}
will become : X X Repeat(i) {X X Call(a)}
        a : Repeat(j) {X X}*)
let transform_recursive_repeat_ast globalAst =
  let Ast(i, s, blocks) = globalAst in
  let aux b final = (create_recursive_repeat_blocks b)@final in
  Ast(i, s, (List.fold_right (aux) blocks []))

(*Takes an ast
transforme ast to completely remove blocks that are never called,
never referenced and never referenced in songs*)
let transform_remove_unused_blocks globalAst = 
  let Ast(i, s, blocks) = globalAst in
  let rec aux_iterate_on_blocks tailingBlocks =
    match tailingBlocks with
    |[] -> []
    |h::q -> let analyseUsage = analyse_block h globalAst in
          if analyseUsage.callCounter + analyseUsage.jumpCounter + analyseUsage.anonymousCounter + analyseUsage.songReferenceCounter <= 0 then
            let Block(Id(id), _) = h in
            Printf.printf "Removing block \"%s\" never used\n" id;
            aux_iterate_on_blocks q
          else
            h::aux_iterate_on_blocks q
  in
  Ast(i, s, aux_iterate_on_blocks blocks)




let transform_ast globalAst =
  let ast = transform_anonymous_and_call_ast globalAst in
  let ast = transform_remove_unused_blocks ast in
  let ast = transform_recursive_repeat_ast ast in
  ast

(*---------------------------------------------------------------*)
(*--------------------   debug functions    ---------------------*)
(*---------------------------------------------------------------*)


let debug globalAstTest =
  let Ast(instruments, songs, blocks) = globalAstTest in
  let rec aux bs =
    match bs with
    |h::q -> let Block(Id(id), _) = h in
              let res = analyse_block h globalAstTest in Printf.printf "B[%s] : nbCall=%i | nbJump=%i | nbAnonymous=%i | size=%i | nbRefsInSongs=%i\n" id res.callCounter res.jumpCounter res.anonymousCounter res.blockSize res.songReferenceCounter; aux q
    |_ -> ()
  in
  aux blocks