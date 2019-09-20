(* Note: You may introduce new code anywhere in this file. *) 

type room_id = string
type exit_name = string
exception UnknownRoom of room_id
exception UnknownExit of exit_name

(* TODO: replace [unit] with a type of your own design. *)
type t = unit

let from_json json = 
  failwith "Unimplemented"

let start_room adv =
  failwith "Unimplemented"

let room_ids adv = 
  failwith "Unimplemented"

let description adv room =
  failwith "Unimplemented"

let exits adv room = 
  failwith "Unimplemented"

let next_room adv room ex =
  failwith "Unimplemented"

let next_rooms adv room =
  failwith "Unimplemented"
