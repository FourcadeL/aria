(*Post parsing and Pre compilation
transofrmation of ast for anonymous replacement*)
open Ast

(*Counter struct for number of call and number of anonymous places*)
type analyseCounter = {
  callCounter : int; (*number af call to that block*)
  jumpCounter : int; (*number of jumpts to that block*)
  anonymousCounter : int; (*number of anonymous calls to that block*)
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
let analyseBlock block globalAst =
  let Block(Id(id), _)  = block in
  let auxCountInBlock idToTest accCounter block =
    let Block(_, astB) = block in
    let rec interAuxIterateAst idToTestInner ast counter =
      match ast with
      |Seq(a1, a2) -> let c1 = interAuxIterateAst id a1 counter in interAuxIterateAst id a2 c1
      |Call(BlockId(Id(s))) when s = idToTestInner -> {counter with callCounter = counter.callCounter + 1}
      |Jump(BlockId(Id(s))) when s = idToTestInner -> {counter with jumpCounter = counter.jumpCounter + 1}
      |BlockId(Id(s)) when s = idToTestInner -> {counter with anonymousCounter = counter.anonymousCounter + 1}
      |Repeat(_, a) -> interAuxIterateAst idToTestInner a counter
      |Transpose(_, a) -> interAuxIterateAst idToTestInner a counter
      |WithVolume(_, a) -> interAuxIterateAst idToTestInner a counter
      |WithInstrument(_, a) -> interAuxIterateAst idToTestInner a counter
      |Loop(a) -> interAuxIterateAst idToTestInner a counter
      |_ -> counter
    in
  interAuxIterateAst idToTest astB accCounter
  in
  let baseCounter = {callCounter = 0; jumpCounter = 0; anonymousCounter = 0; blockSize = getAstBlockSizeScore block} in
  let Ast(_, _, blocksList) = globalAst in
  List.fold_left (auxCountInBlock id) baseCounter blocksList


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




(*---------------------------------------------------------------*)
(*--------------------      functions       ---------------------*)
(*---------------------------------------------------------------*)

let transform_ast globalAst =
  let Ast(instruments, songs, blocks) = globalAst in
  let rec aux_iterate_on_blocks currentGlobalAst blocks =
    match blocks with
    |[] -> currentGlobalAst
    |h::q -> let analyseUsage = analyseBlock h globalAst in
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




(*---------------------------------------------------------------*)
(*--------------------   debug functions    ---------------------*)
(*---------------------------------------------------------------*)


let debug globalAstTest =
  let Ast(instruments, songs, blocks) = globalAstTest in
  let rec aux bs =
    match bs with
    |h::q -> let Block(Id(id), _) = h in
              let res = analyseBlock h globalAstTest in Printf.printf "B[%s] : nbCall=%i | nbJump=%i | nbAnonymous=%i | size=%i\n" id res.callCounter res.jumpCounter res.anonymousCounter res.blockSize; aux q
    |_ -> ()
  in
  aux blocks