(* Note: You may introduce new code anywhere in this file. *) 
open Adventure

type t = {
  current_room : room_id;
  visited_rooms : room_id list;
}

let init_state (adv : Adventure.t)  = {
  current_room = start_room adv;
  visited_rooms = [start_room adv];  
}

let current_room_id st =
  st.current_room

let visited st =
  st.visited_rooms

type result = Legal of t | Illegal

(** [update_room ex adv st] returns the room_id of the room
to which the player exits, if it exists. It raises an Illegal exception
if the exit is not available from the player's current room. *)
(*let update_room ex adv st =
  try (get_rooms adv) |> List.find (fun {id} -> id = st.current_room)
   |> get_exits |> List.find (fun {name} -> name = ex) |> get_room_id with
    | Not_found -> raise Illegal*)

(** [update_visited lst room] prepends [room] to [lst] if it is not
currently in lst. This function simply returns [lst] if [room] is 
already in [lst]. *)
let update_visited lst room =
  if List.mem room lst then lst
  else room :: lst

let go ex adv st =

  let current_room = try next_room adv st.current_room ex with
    | (UnknownExit _) -> st.current_room in
  let visited_rooms = update_visited st.visited_rooms current_room in

  let st' = {
    current_room = current_room;
    visited_rooms = visited_rooms;
  } in

  if current_room = st.current_room then Illegal
  else Legal st'

let update_state old_state result =
  match result with
    | Illegal -> old_state
    | Legal t -> t  