(* Note: You may introduce new code anywhere in this file. *) 
open Adventure

exception UnknownRelic of relic_name

type room_state = room_id * relic_name list

(** The abstract type of values representing states. *)
type t = {
  current_room : room_id;
  visited_rooms : room_id list;
  player_score : int;
  inventory : relic_name list;
  room_information : room_state list;
  treasure_items : int;
}

let rec get_room_states acc adv (room_list : room_id list) =
  match room_list with
    | [] -> acc
    | h::t -> get_room_states ((state_of_room adv h) :: acc) adv t

(** [init_state adv] takes an adventure and creates a state based on the initial
information from [adv]. *)
let init_state (adv : Adventure.t)  = {
  current_room = start_room adv;
  visited_rooms = [start_room adv]; 
  player_score = 0;
  inventory = [];
  room_information = (get_room_states [] adv (room_ids adv));
  treasure_items = 0
}

let current_room_id st =
  st.current_room

let current_room_loot st =
  List.assoc st.current_room st.room_information

let visited st =
  st.visited_rooms

let current_score st = 
  st.player_score

let current_inventory st = 
  st.inventory

let current_room_info st =
  st.room_information

(** [current_progress st] is the number of items the player has successfully
dropped in the treasure room at the time the function is called. *)
let current_progress st =
  st.treasure_items

type result = Legal of t | Illegal | Win

(** [relics_from_rooms acc assoc_lst] takes an assoc_list and 
returns a list of the values bound to each key as a list. *)
let rec relics_from_rooms acc assoc_lst =
  match assoc_lst with
  | [] -> acc
  | (a,b) :: t -> relics_from_rooms (b :: acc) t

(*let total_items adv = 
  let original = adv |> init_state in
  let rooms = original.room_information in
  let all = relics_from_rooms [] rooms in
  let relics = List.filter (fun x -> x <> []) all in
  List.length relics *)
  
(** [revert_list lst] takes a string list and convert it to a single string. *)
let revert_list lst = 
  let new_str = List.fold_left (fun p n -> p ^ " " ^ n) "" lst in
  String.trim new_str

(** [split_list acc lst] converts a list of strings into a list
of string lists, where each string in the original [lst] becomes a
list containing that string. *)
let rec split_list (acc : string list list) (lst : string list) =
  match lst with 
  | [] -> acc
  | h :: t -> split_list ((h :: []) :: acc) t

(** [split_list_list acc lst] converts a list of string lists
into a list of lists of string lists. Each element of each original string
list is concatenated with the empty list to make a string list. *)
let rec split_list_list (acc : (string list list) list) (lst : string list list) =
  match lst with
  | [] -> acc
  | h :: t -> split_list_list ((split_list [] h) :: acc) t

(** [break_list acc lst] converts a list of string lists into 
a main list of the strings within each sub-list. *)
let rec string_from_list acc lst =
  match lst with 
  | [] -> acc
  | h :: t -> string_from_list ((revert_list h) :: acc) t

(** [relic_exists st rel lst] determines if [rel] can be found in
the assoc_list of room_information of [st]. *)
let relic_exists st rel = 
  let bracketed = relics_from_rooms [] (current_room_info st) in
  let list_list = split_list_list [] bracketed in
  let sep_list = List.concat list_list in
  let exposed = string_from_list [] sep_list in
  List.mem rel exposed

let total_items adv =
  let original = adv |> init_state in 
  let bracketed = relics_from_rooms [] (current_room_info original) in
  let list_list = split_list_list [] bracketed in
  let sep_list = List.concat list_list in
  let exposed = string_from_list [] sep_list in
  List.length exposed


(** [update_visited lst room] prepends [room] to [lst] if it is not
currently in lst. This function simply returns [lst] if [room] is 
already in [lst]. *)
let update_visited lst room =
  if List.mem room lst then lst
  else room :: lst

(** [update_score lst room adv current_score] adds the [score] of [room]
to the player's [current_score] if the [room] is not in [lst] of visited rooms. 
This function simply returns [current_score] if the [room] is already in 
[lst] of visited rooms. *)
let update_score lst room adv current_score =
  if List.mem room lst then current_score
  else current_score + (room_score adv room)

let take_item st rel =

  let room_relics = List.assoc st.current_room st.room_information in

  let filtered_room = try List.filter (fun x -> x <> rel) room_relics with
                      | _ -> room_relics in 

  let new_room_info = if filtered_room = room_relics then st.room_information
                      else 
                      let old_list = List.assoc st.current_room 
                      st.room_information in
                      let new_list = List.filter (fun x -> x <> rel) old_list in
                      let updated = List.remove_assoc st.current_room 
                      st.room_information in
                      let new_state = (st.current_room, new_list) in
                      new_state :: updated
  in

  let new_inv = if List.mem rel st.inventory || 
                not (relic_exists st rel) then st.inventory
                else rel :: st.inventory in 

  let st' = {
    current_room = st.current_room;
    visited_rooms = st.visited_rooms;
    player_score = st.player_score;
    inventory = new_inv;
    room_information = new_room_info;
    treasure_items = st.treasure_items;
  } in

  if new_inv = st.inventory then Illegal
  else Legal st'

(** [from_inv_to_room adv st rel] is the result of the player attempting to
drop [rel] given [st]. *)
let from_inv_to_room adv st rel = 

  let inventory_relic = try List.find (fun x -> x = rel) st.inventory with
                        | Not_found -> "none"
  in

  let new_rooms = if inventory_relic = "none" then st.room_information else
                    let old_list = List.assoc st.current_room 
                    st.room_information in
                    let new_list = if List.mem inventory_relic old_list ||
                                    not (List.mem inventory_relic st.inventory)
                                  then old_list else inventory_relic :: old_list
                                  in
                    let updated = List.remove_assoc st.current_room 
                    st.room_information in
                    let new_room_state = (st.current_room, new_list) in
                    new_room_state :: updated
  in

  let new_inv = List.filter (fun x -> x <> rel) st.inventory in

  let st' = {
    current_room = st.current_room;
    visited_rooms = st.visited_rooms;
    player_score = st.player_score;
    inventory = new_inv;
    room_information = new_rooms;
    treasure_items = st.treasure_items;
  } in

  if new_rooms = st.room_information then Illegal else Legal st'

let drop_item adv st rel = 
  try  
    if st.current_room = treasure_room adv then
      let inventory_relic = try List.find (fun x -> x = rel) st.inventory with
                          | Not_found -> "none"
      in

      let new_inv = if inventory_relic = "none" then st.inventory else
                    List.filter (fun x -> x <> rel) st.inventory in

      let new_score = st.player_score + (relic_points adv rel) in
      let new_treasure_items = if inventory_relic = "none" then st.treasure_items
                               else st.treasure_items + 1 in

      let st' = {
        current_room = st.current_room;
        visited_rooms = st.visited_rooms;
        player_score = new_score;
        inventory = new_inv;
        room_information = st.room_information; 
        treasure_items = new_treasure_items;
      }
      in

      if new_treasure_items >= total_items adv then Win else
        if new_inv = st.inventory then Illegal else Legal st'

  else
    from_inv_to_room adv st rel

  with
    | Not_found -> Illegal

let go ex adv st =

  let current_room = try next_room adv st.current_room ex with
    | (UnknownExit _) -> st.current_room in
  let visited_rooms = update_visited st.visited_rooms current_room in

  let player_score = try update_score st.visited_rooms current_room 
      adv st.player_score with
      | (UnknownRoom _) -> st.player_score in

  let st' = {
    current_room = current_room;
    visited_rooms = visited_rooms;
    player_score = player_score;
    inventory = st.inventory;
    room_information = st.room_information;
    treasure_items = st.treasure_items
  } in

  if current_room = st.current_room then Illegal
  else Legal st'

let update_state old_state result =
  match result with
    | Illegal -> old_state
    | Legal t -> t  
    | Win -> old_state