open OUnit2
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

(** [make_start_room_test name adv expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [start_rom adv]. *)
let make_start_room_test 
    (name : string)
    (adv : Adventure.t)
    (expected_output : room_id) : test =
  name >:: (fun _ -> 
    assert_equal expected_output (start_room adv))

(** [make_room_ids_test name adv expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [room_ids adv]. *)
let make_room_ids_test
    (name : string)
    (adv : Adventure.t)
    (expected_output : room_id list) : test =
  name >:: (fun _ -> 
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          expected_output (room_ids adv))

(** [make_description_test name adv room expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [description adv room]. *)
let make_description_test 
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : string) : test =
  name >:: (fun _ -> 
    assert_equal expected_output (description adv room))

(** [make_exits_test name adv room expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [exits adv room]. *)
let make_exits_test 
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : exit_name list) : test =
  name >:: (fun _ ->
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          expected_output (exits adv room))

(** [make_next_room_test name adv room ex expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [next_room adv room ex]. *)
let make_next_room_test 
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (ex : exit_name)
    (expected_output : room_id) : test =
  name >:: (fun _ ->
    assert_equal expected_output (next_room adv room ex))

(** [make_next_rooms_test name adv room expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [next_rooms adv room]. *)
let make_next_rooms_test 
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : room_id list) : test =
  name >:: (fun _ -> 
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          expected_output (next_rooms adv room))

let fake_room : room_id = "fake room"
let fake_exit : exit_name = "fake exit"
let lonely_room = from_json (Yojson.Basic.from_file "lonely_room.json")
let ho_plaza = from_json (Yojson.Basic.from_file "ho_plaza.json")

let adventure_tests =
  [
    make_start_room_test "Start of lonely_room is 'the room'"
      lonely_room "the room";

    make_start_room_test "Start of ho_plaza is 'ho plaza'" ho_plaza "ho plaza";

    make_room_ids_test "IDs of lonely_room are ['the room']"
      lonely_room ["the room"];

    make_room_ids_test "IDS of ho_plaza are correct"
      ho_plaza ["nirvana"; "tower"; "health"; "ho plaza"];

    make_description_test "Description of 'the room' is 'A very lonely room.'"
      lonely_room "the room" "A very lonely room.";

    make_description_test "Description of 'ho plaza' is correct" ho_plaza 
    "ho plaza" "You are on Ho Plaza. Cornell Health is to the southwest. \
    The chimes are playing a concert in the clock tower. \
    Someone tries to hand you a quartercard, but you avoid them.";

    make_description_test "Description of 'health' is correct" ho_plaza "health"
    "You are at the entrance to Cornell Health. \
    A sign advertises free flu shots. \
    You briefly wonder how long it would take to get an appointment. \
    Ho Plaza is to the northeast.";

    make_description_test "Description of 'tower' is correct" ho_plaza "tower"
    "You climbed up all 161 steps to the top of McGraw Tower. \
    A Chimesmaster is playing the Jennie McGraw Rag. \
    You feel inspired to ascend higher.";

    make_description_test "Description of 'nirvana' is correct" ho_plaza
    "nirvana" "You have reached a higher level of existence.  \
    There are no more words.";

    "description should raise UnknownRoom exception for invalid room" >::
      (fun _ -> assert_raises (UnknownRoom fake_room)
        (fun () -> description ho_plaza fake_room));

    make_exits_test "Exits of 'the room''should be []"
      lonely_room "the room" [];

    make_exits_test "Exits of 'tower' should be correct"
      ho_plaza "tower" ["down"; "back"; "Ho Plaza"; "higher"];
    "exits should raise UnknownRoom exception for invalid room" >::
      (fun _ -> assert_raises (UnknownRoom fake_room)
        (fun () -> exits ho_plaza fake_room));

    make_next_room_test "'southwest' exit of ho_plaza should be 'health'"
      ho_plaza "ho plaza" "southwest" "health";

    "next_room should raise UnknownRoom exception for invalid room" >::
      (fun _ -> assert_raises (UnknownRoom fake_room)
        (fun () -> next_room ho_plaza fake_room "ho plaza"));

    "next_room should raise UnknownExit exception for lonely_room" >::
      (fun _ -> assert_raises (UnknownExit fake_room)
        (fun () -> next_room lonely_room "the room" fake_room));

    make_next_rooms_test "exit ids of 'the room' should be []"
      lonely_room "the room" [];

    make_next_rooms_test "exit ids of 'ho plaza' should be correct"
      ho_plaza "ho plaza" ["tower"; "health"];

    "next_rooms should raise UnknownRoom exception for invalid room" >::
      (fun _ -> assert_raises (UnknownRoom fake_room)
        (fun () -> next_rooms ho_plaza fake_room));
  ]

(** [make_parse_test name str expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [parse str]. *)
let make_parse_test
    (name : string)
    (str : string)
    (expected_output : command) : test =
  name >:: (fun _ ->
    assert_equal expected_output (parse str))

let command_tests =
  [
    make_parse_test "parse 'go clock tower' yields Go ['clock'; 'tower']"
      "go clock tower" (Go ["clock"; "tower"]);

    make_parse_test "parse '  go  clock    tower ' yields Go ['clock'; 'tower']"
      "   go  clock    tower " (Go ["clock"; "tower"]);

    make_parse_test "parse 'quit' yields Quit" "quit" Quit;

    "parse 'quit clock tower' raises Malformed exception" >::
      (fun _ -> assert_raises Malformed
        (fun () -> parse "quit clock tower"));

    "parse 'go' raises Malformed exception" >::
      (fun _ -> assert_raises Malformed
        (fun () -> parse "go"));

    "parse '' raises Empty exception" >::
      (fun _ -> assert_raises Empty
        (fun () -> parse ""));

    "parse '    ' raises Empty exception" >::
      (fun _ -> assert_raises Empty
        (fun () -> parse "    "));
  ]

(** [make_exits_test name adv room expected_start expected_visited]
constructs an OUnit test named [name] that asserts the equality of 
[expected_start] with [adv |> init_state |> current_room_id] and asserts 
the equality of [expected_visited] with [adv |> init_state_visited]. *)
let make_init_state_test
    (name : string)
    (adv : Adventure.t)
    (expected_start: room_id)
    (expected_visited : room_id list) : test =
  name >::  (fun _ ->
    assert_equal expected_start (adv |> init_state |> current_room_id);
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_visited (adv |> init_state |> visited))

(** [make_go_test name ex adv room expected_new_curr expected_new_visit]
constructs an OUnit test named [name] that asserts the equality of
[expected_new_curr] with 
[adv
  |> init_state 
  |> go ex adv 
  |> update_state st
  |> current_room_id]
and asserts the equality of [expected_new_vist] with 
[adv
  |> init_state
  |> go ex adv
  |> update_state st
  |> visited]. *)
let make_go_test
    (name : string)
    (ex : Adventure.exit_name)
    (adv : Adventure.t)
    (st : State.t)
    (expected_new_curr : room_id)
    (expected_new_visit : room_id list) : test =
  name >:: (fun _ ->
    assert_equal expected_new_curr 
    (adv
      |> init_state
      |> go ex adv
      |> update_state st
      |> current_room_id);
    assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_new_visit
    (adv
      |> init_state 
      |> go ex adv
      |> update_state st 
      |> visited))

let lonely_room_init = init_state lonely_room
let ho_plaza_init = init_state ho_plaza

let lonely_exits = lonely_room |> init_state |> go fake_exit lonely_room
let plaza_exit_fake = ho_plaza |> init_state |> go fake_exit ho_plaza

let state_tests =
  [
    make_init_state_test "start room of lonely_room is 'the room'"
    lonely_room "the room" ["the room"];

    make_init_state_test "start room of ho_plaza is 'ho plaza'"
    ho_plaza "ho plaza" ["ho plaza"];

    "go from lonely_room should return Illegal" >::
      (fun _ -> assert_equal Illegal lonely_exits);

    "go from ho_plaza to fake_room should return Illegal" >::
      (fun _ -> assert_equal Illegal plaza_exit_fake);

    make_go_test "from ho plaza to tower" "chimes" ho_plaza
    (ho_plaza |> init_state) "tower" ["tower"; "ho plaza"];
    
    make_go_test "from ho plaza to health back to ho plaza" "northeast" ho_plaza
    (ho_plaza
      |> init_state
      |> go "southwest" ho_plaza
      |> update_state ho_plaza_init
      |> go "northeast" ho_plaza
      |> update_state ho_plaza_init) "ho plaza" ["health"; "ho plaza"];
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
