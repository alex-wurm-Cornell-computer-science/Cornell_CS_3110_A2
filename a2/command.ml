open Adventure
(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Quit
  | Score
  | Take of object_phrase
  | Drop of object_phrase
  | Inventory
  | Loot

exception Empty

exception Malformed

(** [format_command lst] takes a list of strings and raises an [Empty] 
  exception if the list is nil. If the list of strings represents
  a properly formed command - one of the following: "go" followed by a room_id,
  "quit", "score", "take" followed by a [relic_name], "drop" followed by a
  [relic_name] inventory", or "loot" - [format_command lst] returns the verb &
  object_phrase. If the command is not properly formed, [format_command lst] 
  raises a [Malformed] exception. *)
let format_command lst =
  match lst with
    | [] -> raise (Empty)
    | h :: t when h = "go" -> if t = [] then raise (Malformed) else Go t
    | h :: t when h = "quit" -> if t = [] then Quit else raise (Malformed) 
    | h :: t when h = "score" -> if t = [] then Score else raise (Malformed)
    | h :: t when h = "take" -> if t = [] then raise (Malformed) else Take t
    | h :: t when h = "drop" -> if t = [] then raise (Malformed) else Drop t
    | h :: t when h = "inventory" -> if t = [] then Inventory else raise (Malformed)
    | h :: t when h = "loot" -> if t = [] then Loot else raise (Malformed)
    | _ -> raise (Malformed)

let parse str =
  let comm = String.trim str in
  let comm_list = String.split_on_char ' ' comm in
  let filtered_list = List.filter (fun x -> (x = "") == false) comm_list in
  format_command filtered_list
  
