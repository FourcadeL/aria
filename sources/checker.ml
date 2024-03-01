open Audio
open BinaryTranslator


let check_block_size block =
  let Block(Id(blockName), instructions) = block in
  let rec aux inst_list acc =
    match inst_list with
    |[] -> true
    |h::t -> let newSize = (acc + get_instruction_byte_size h) in
      if (newSize > 256) then failwith ("instruction block ["^blockName^"] exceeds maximum size of 256 bytes") else (aux t newSize)
  in
  aux instructions 0

let rec check_blocks_size blocks =
  match blocks with
  |[] -> true
  |h::t -> if check_block_size h then check_blocks_size t else false;;

let rec check_identifier_declared identifier blocks =
  match blocks with
  |[] -> false
  |h::t -> let Block(tmpIdentifier, _) = h in
            if tmpIdentifier = identifier then true else check_identifier_declared identifier t

let check_song_pointers song blocks =
  let Song(Id(songIdentifier), Channel(ch1Identifier), Channel(ch2Identifier), Channel(ch3Identifier), Channel(ch4Identifier)) = song in
  if not (check_identifier_declared ch1Identifier blocks) then
    let Id(tmpString) = ch1Identifier in Printf.printf "identifier [%s] in song [%s] is not defined\n" tmpString songIdentifier;
  if not (check_identifier_declared ch2Identifier blocks) then
    let Id(tmpString) = ch2Identifier in Printf.printf "identifier [%s] in song [%s] is not defined\n" tmpString songIdentifier;
  if not (check_identifier_declared ch3Identifier blocks) then
    let Id(tmpString) = ch3Identifier in Printf.printf "identifier [%s] in song [%s] is not defined\n" tmpString songIdentifier;
  if not (check_identifier_declared ch4Identifier blocks) then
    let Id(tmpString) = ch4Identifier in Printf.printf "identifier [%s] in song [%s] is not defined\n" tmpString songIdentifier;;

let rec check_songs_pointers songs blocks =
  match songs with
  |[] -> ()
  |h::t -> check_song_pointers h blocks; check_songs_pointers t blocks