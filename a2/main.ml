open Yojson.Basic.Util
open Adventure
open Command
open State

(** [user_input _] prompts the user for a command. If the command
is empty the game will inform the player and ask for another command. 
If the command was malformed, the game will inform the player and ask
for another command. *)
let rec user_input _ = 
  try parse (read_line ()) with
      | Empty -> print_string "No command was entered, please try again. \n";
        user_input()
      | Malformed -> print_string "Command unclear, please try again. \n"; 
        user_input()

(** [get_adv f] converts input file f to an Adventure. If any exceptions
are raised from the conversion, the game will notify the player and ask for
a new file name input. *)
let rec get_adv f = try f |> Yojson.Basic.from_file |> from_json with
                    | _ -> print_string "Invalid adventure. Try again. \n";
                          get_adv (read_line ())

(** [update_desc adv st] prints the description of the [current_room] given
the state and the adventure being played. *)
let update_desc adv st = 
  print_string "\n";
  st |> current_room_id |> description adv |> print_string;
  print_string "\n"

(** [revert_exit lst] takes a string list and convert it to a single string. *)
let revert_exit lst = 
  let new_str = List.fold_left (fun p n -> p ^ " " ^ n) "" lst in
  String.trim new_str

(** [interp_command adv st command] allows the user to play the game by
printing an exit message if the input command is [Quit] or by inspecting a 
[Go] message to determine what action to perform. If the command is [Legal]
the state is updated and the user is prompted for another command. If the 
command is [Illegal] the game prints an error message and asks the user
for a new command. *)
let rec interp_command adv st command = 
  match command with
  | Quit -> print_string "Thank you for playing the Adventure Game Engine! \n"
  | Go e -> match go (revert_exit e) adv st with 
            | Legal st' -> continue_game adv st (Legal st')
            | Illegal -> print_string "Illegal destination. \
              Please try again. \n"; interp_command adv st (user_input ())

(** [continue_game adv st result] updates the state of the game, prints the
description, and prompts the user for another command to continue the game. *)
and continue_game adv st result =
  let st' = update_state st result in
  update_desc adv st';
  interp_command adv st' (user_input ())
 
(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  (*failwith "Unimplemented"*)
  let adv = get_adv f in
  let st = init_state adv in
  update_desc adv st;

  let response = user_input () in

  let _ = interp_command adv st (response) in

  Stdlib.exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
