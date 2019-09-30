(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Quit
  | Score

exception Empty

exception Malformed

(** [format_command lst] takes a list of strings and raises an [Empty] 
  exception if the list is nil. If the list of strings represents
  a properly formed command - either "go" followed by a room_id or 
  just "quit" - [format_command lst] returns the verb & object_phrase.
  If the command is not properly formed, [format_command lst] 
  raises a [Malformed] exception. *)
let format_command lst =
  match lst with
    | [] -> raise (Empty)
    | h :: t when h = "go" -> if t = [] then raise (Malformed) else Go t
    | h :: t when h = "quit" -> if t = [] then Quit else raise (Malformed) 
    | h :: t when h = "score" -> if t = [] then Score else raise (Malformed)
    | _ -> raise (Malformed)

let parse str =
  let comm = String.trim str in
  let comm_list = String.split_on_char ' ' comm in
  let filtered_list = List.filter (fun x -> (x = "") == false) comm_list in
  format_command filtered_list
  
