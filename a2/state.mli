(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type representing room states. *)
type room_state = string * string list

(** The abstract type of values representing the game state. *)
type t 

(** [init_state a] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Adventure.t -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_room_id : t -> string

(** [visited st] is a set-like list of the room identifiers the adventurer has 
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)
val visited : t -> string list

(** [current_score st] is the player's score at the time the command is issued.
 The adventurer's score increases each time they visit a new room or 
 bring an item to the treasure room. *)
val current_score : t -> int

(** [current_inventory st] is the list of the items in the player's [inventory] 
at the time the command is issued. The adventurer's inventory changes each time
they [Take] or [Drop] an item. *)
val current_inventory : t -> Adventure.relic_name list


(** [current_room_info st] is the list of tuples of [room_ids] and 
[relic_name lists] representing the [room_states] of [st]. *)
val current_room_info : t -> room_state list

(** The type representing the result of an attempted action. *)
type result = Legal of t | Illegal | Win

(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** [update_state old_state result] is the state that results from command go.
If the command is [Illegal] then the previous state is returned. If the 
comamnd is [Legal t] the new state [t] is returned. *)
val update_state : t -> result -> t

(** [take_item st rel] is the result of trying to take the relic [rel]
from the [current_room] in [st]. *)
val take_item : t -> Adventure.relic_name -> result

(** [drop_item adv st rel] is the result of trying to drop the 
relic [rel] into the [current_room] in [st]. *)
val drop_item : Adventure.t -> t -> Adventure.relic_name -> result

(* For Testing *)

(** [get_room_states acc adv (room_list : room_id list)] is the list of
[room_states] as tuples of all of the [rooms] in [adv]. *)
val get_room_states : room_state list -> Adventure.t -> 
Adventure.room_id list -> room_state list

(** [current_room_loot st] is the list of [relic_names] in 
[loot] of the current room. *)
val current_room_loot : t -> Adventure.relic_name list

(** [string_from_list acc lst] takes a list containing string lists
and returns a single list of all of the sub-strings. *)
val string_from_list : string list -> string list list -> string list

(** [total_items adv] is the total number of items in the initiae state
generated for [adv]. *)
val total_items : Adventure.t -> int