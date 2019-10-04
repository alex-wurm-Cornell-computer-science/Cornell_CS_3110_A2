(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** The type of relic names. *)
type relic_name = string

(** The abstract type of values representing exits. *)
type exit = {
  name : exit_name;
  room_id : room_id;
}

(** The abstract type of values representing relics. *)
type relic = {
  relic_name : relic_name;
  points : int;
}

(** The abstract type of values representing rooms. *)
type room = {
  id : room_id;
  description : string;
  exits : exit list;
  score : int;
  loot : relic list;
}

(** The abstract type of values representing adventures. *)
type t = {
  rooms : room list;
  start_room : room_id;
  relics : relic list;
  treasure_room : room_id;
}

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** [treasure_room a] is the identifier of the treasure room in adventure
[a]. *)
val treasure_room : t -> room_id

(** [room_score a r] is the score of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val room_score : t -> room_id -> int

(** [list_relic_names room] is the set-like list of all of the [relic-names]
of the [loot] in [room]. *)
val list_relic_names : room -> relic_name list

(** [relic_names a] is a set-like list of all of the relic names in 
    adventure [a]. *)
val relic_names : t -> room_id -> relic_name list

(** [state_of_room adv room] is the tuple [(room, room_relics)] where 
[room_relics] is the list [loot] of [relics] in [room]. *)
val state_of_room : t -> room_id -> (room_id * relic_name list)

(** [relic_points adv rel] is the point value attributed with the relic named
[rel] in adventure [adv]. *)
val relic_points : t -> relic_name -> int