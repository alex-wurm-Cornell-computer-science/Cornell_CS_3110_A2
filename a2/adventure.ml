(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type room_id = string
type exit_name = string
type relic_name = string
exception UnknownRoom of room_id
exception UnknownExit of exit_name

type exit = {
  name : exit_name;
  room_id : room_id;
}

type relic = {
  relic_name : relic_name;
  points : int;
}

type room = {
  id : room_id;
  description : string;
  exits : exit list;
  score : int;
  loot : relic list;
}

type t = {
  rooms : room list;
  start_room : room_id;
  relics : relic list;
  treasure_room : room_id;
}

(** [exit_of_json j] is the record with fields name and room_id
that represents an [exit] [j]. *)
let exit_of_json j = {
  name = j |> member "name" |> to_string;
  room_id = j |> member "room id" |> to_string;
}

(** [relic_of_json j] is the record with fields [relic_name] and [points]
that represents a [relic] [j]. *)
let relic_of_json j = {
  relic_name = j |> member "relic name" |> to_string;
  points = j |> member "points" |> to_int;
}

(** [room_of_json j] is the record with fields [id], [description], [exits],
[score], and [loot] that represents a [room] [j]. *)
let room_of_json j = {
  id = j |> member "id" |> to_string;
  description = j |> member "description" |> to_string;
  exits = j |> member "exits" |> to_list |> List.map exit_of_json;
  score = j |> member "score" |> to_int;
  loot = j |> member "loot" |> to_list |> List.map relic_of_json;
}

let from_json json = {
  rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
  start_room = json |> member "start room" |> to_string;
  relics = json |> member "relics" |> to_list |> List.map relic_of_json;
  treasure_room = json |> member "treasure room" |> to_string;
}

let start_room adv = 
  adv.start_room

(** [get_ids acc (room_list : room list)] is the list of [ids]
of all of the [rooms] in [room_list]. *)
let rec get_ids acc (room_list : room list) =
  match room_list with
    | [] -> acc
    | h::t -> get_ids (h.id :: acc) t

let room_ids adv =
  get_ids [] adv.rooms

(** [room_exists adv room] is the boolean evaluation of the statement
"[room] is one of the [rooms] in [adv]". *)
let room_func adv room func =
  try adv.rooms |> List.find (fun {id} -> id = room) |> func with
    | Not_found -> raise (UnknownRoom room)

(** [get_id room] is the [id] of [room]. *)
let get_id room = room.id

(** [get_description room] is the [description] of [room]. *)
let get_description room = room.description

let description adv room = 
  room_func adv room get_description

(** [get_names acc (exit_list : exit list)] is the list of [names]
of [exit_list] *)
let rec get_names acc (exit_list : exit list) =
  match exit_list with
    | [] -> acc
    | h::t -> get_names (h.name :: acc) t

(** [get_room_ids acc (exit_list : exit list)] is the list of [room_ids]
of [exit_list] *)
let rec get_room_ids acc (exit_list : exit list) =
  match exit_list with
    | [] -> acc
    | h::t -> get_room_ids (h.room_id :: acc) t

(** [get_exits room] is the [exit list] of [room]. *)
let get_exits room = room.exits

(** [get_room_id exit] is the [room_id] of [exit]. *)
let get_room_id exit = exit.room_id

(** [list_exit_names room] is the set-like list of exit names of [room]. *)
let list_exit_names room =
  room |> get_exits |> get_names [] |> List.sort_uniq compare

(** [list_exit_ids room] is the set-like list of ids of exits of [room]. *)
let list_exit_ids room = 
    room |> get_exits |> get_room_ids [] |> List.sort_uniq compare

let exits adv room = 
  room_func adv room list_exit_names

let next_room adv room ex = 
  let r = 
    try adv.rooms |> List.find (fun {id} -> id = room) with
      | Not_found -> raise (UnknownRoom room)
  in

  let e =
    try r |> get_exits |> List.find (fun {name} -> name = ex) with
      | Not_found -> raise (UnknownExit ex)
  in

  get_room_id e

let next_rooms adv room =
  try adv.rooms |> List.find (fun {id} -> id = room) |> list_exit_ids with
    | Not_found -> raise (UnknownRoom room)

(**Additional functions for A3 functionality added below: *)

let treasure_room adv = adv.treasure_room

(** [get_score room] is the [score] of [room]. *)
let get_score room = room.score

let room_score adv room = 
  room_func adv room get_score

(** [get_relics acc (relic_list : relic list)] is the list of [relic_names]
of all of the [relics] in [relic_list]. *)
let rec get_relics acc (relic_list : relic list) =
  match relic_list with
    | [] -> acc
    | h::t -> get_relics (h.relic_name :: acc) t

(** [all_relics adv] is the list of all of the items in the adventure
[adv]. *)
let all_relics adv = get_relics [] adv.relics

(** [room_loot room] is the list of all of the items in the room [room]. *)
let room_loot room = room.loot

let list_relic_names room =
  room |> room_loot |> get_relics [] |> List.sort_uniq compare

let relic_names adv room =
  room_func adv room list_relic_names

let state_of_room adv room =
  let room_relics = adv.rooms 
                    |> List.find (fun {id} -> id = room) 
                    |> list_relic_names in
  (room, room_relics)

let relic_points adv rel = 
  let item = List.find (fun {relic_name} -> relic_name = rel) adv.relics in
  item.points