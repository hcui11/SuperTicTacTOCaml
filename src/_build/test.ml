open OUnit2
open Board
open Command
open Monte
open Play
open State
open Unix

let test_index_of_coordinate
    (name : string)
    (x : int)
    (y : int)
    (expected : int) : test =
  name >:: fun _ ->
    assert_equal expected (index_of_coordinate x y) ~printer:string_of_int

let test_board_of_coordinate
    (name : string)
    (x : int)
    (y : int)
    (expected : int) : test =
  name >:: fun _ ->
    assert_equal expected (board_of_coordinate x y) ~printer:string_of_int

let test_game state = 
  let serv = socket PF_INET SOCK_STREAM 0 in
  reset_game state;
  match execute state serv serv Normal Test Hard false with
  | exception _ -> false;
  | x -> true

let test_many_games 
    (name : string)
    (num_tests : int)
    (state : State.t)
    (expected : bool) : test =
  name >:: fun _ -> 
    let games = ref num_tests in
    while !games <> 0 do
      assert_equal expected (test_game state);
      games := !games - 1;
    done

let string_of_square sq =
  match sq with
  | X -> "X"
  | O -> "O"
  | Blank -> " "

let test_tie_breaker
    (name : string)
    (bd : board)
    (expected : square) : test =
  name >:: fun _ ->
    assert_equal expected (tie_breaker bd) ~printer:string_of_square

let test_check_win
    (name : string)
    (bd : board)
    (expected : bool) : test =
  name >:: fun _ ->
    assert_equal expected (check_win bd) ~printer:string_of_bool

let string_of_int_pair_list (pairlst : (int * int) list) : string =
  let strs = List.map
      (fun (a, b) -> (string_of_int a) ^ ", " ^ (string_of_int b)) pairlst in
  String.concat "\n" strs

let test_cartesian_product
    (name : string)
    (l1 : int list)
    (l2 : int list)
    (expected : (int * int) list) : test =
  name >:: fun _ ->
    assert_equal expected (cartesian_product l1 l2)
      ~printer:string_of_int_pair_list

let string_of_command cmd =
  match cmd with
  | Restart -> "restart"
  | Mark (x,y) -> "mark " ^ string_of_int x ^ " " ^ string_of_int y
  | Help -> "help"
  | Quit -> "quit"

let test_parse
    (name : string)
    (str : string)
    (bd : int)
    (expected : command) : test =
  name >:: fun _ ->
    assert_equal expected (parse str bd) ~printer:string_of_command

let test_parse_error
    (name : string)
    (str : string)
    (bd : int)
    (expected : exn) : test =
  name >:: fun _ ->
    assert_raises expected (fun () -> parse str bd)

let unwon_board1 = [|X; O; X; Blank; O; O; X; X; O|]
let unwon_board2 = [|Blank; X; X; Blank; Blank; O; X; Blank; O|]
let unwon_full1 = [|X; O; O; O; X; X; O; X; O|]
let unwon_full2 = [|O; X; X; X; O; O; X; O; X|]
let unwon_mixed1 = [|O; X; Blank; X; O; Blank; X; Blank; X|]
let unwon_mixed2 = [|Blank; X; Blank; Blank; O; O; X; O; Blank|]
let all_X_board = Array.make 9 X
let all_O_board = Array.make 9 O
let checkerboard1 = [|X; O; X; O; X; O; X; O; X|]
let checkerboard2 = [|O; X; O; X; O; X; O; X; O|]
let won_board1 = [||]

let board_tests = 
  [
    test_index_of_coordinate "index of 0 0" 0 0 0;
    test_index_of_coordinate "index of 8 8" 8 8 8;
    test_index_of_coordinate "index of 4 4" 4 4 4;
    test_index_of_coordinate "index of 8 4" 8 4 5;
    test_index_of_coordinate "index of 6 4" 6 4 3;
    test_index_of_coordinate "index of 7 1" 7 1 4;
    test_index_of_coordinate "index of 7 7" 7 7 4;
    test_index_of_coordinate "index of 5 2" 5 2 8;
    test_index_of_coordinate "index of 3 8" 3 8 6;
    test_index_of_coordinate "index of 0 5" 0 5 6;
    test_index_of_coordinate "index of 7 0" 7 0 1;
    test_index_of_coordinate "index of 4 8" 4 8 7;
    test_index_of_coordinate "index of 5 6" 5 6 2;

    test_board_of_coordinate "board of 5 5" 5 5 4;
    test_board_of_coordinate "board of 4 6" 4 6 7;
    test_board_of_coordinate "board of 2 0" 2 0 0;
    test_board_of_coordinate "board of 0 5" 0 5 3;
    test_board_of_coordinate "board of 5 7" 5 7 7;
    test_board_of_coordinate "board of 8 1" 5 7 7;
    test_board_of_coordinate "board of 4 0" 4 0 1;
    test_board_of_coordinate "board of 7 8" 7 8 8;
    test_board_of_coordinate "board of 6 3" 6 3 5;

    test_check_win "checkwin blank" (blank_board ()) false;
    test_check_win "checkwin unwonboard1" unwon_board1 false;
    test_check_win "checkwin unwonboard2" unwon_board2 false;
    test_check_win "checkwin allX" all_X_board true;
    test_check_win "checkwin checkerboard1" checkerboard1 true;
    test_check_win "checkwin checkerboard2" checkerboard2 true;
    test_check_win "checkwin unwonfull1" unwon_full1 false;
    test_check_win "checkwin unwonfull2" unwon_full2 false;
    test_check_win "checkwin unwon_mixed1" unwon_mixed1 false;
    test_check_win "checkwin unwon_mixed2" unwon_mixed2 false;

    test_tie_breaker "tiebreak unwon_full1" unwon_full1 O;
    test_tie_breaker "tiebreak unwon_full2" unwon_full2 X;
  ]

let command_tests = [
  test_parse_error "Empty String Test" "" 9 Empty;
  test_parse_error "Blank String Test" "   " 5 Empty;
  test_parse "Mark Test" "mark             6     5" 5 (Mark (6,5));
  test_parse "Quit Test" " quit    " 3 Quit;
  test_parse "Help Test" "   help " 4 Help;
  test_parse "Restart Test" "restart  " 7 Restart;
  test_parse_error "Invalid Mark Test 1" "mark 5 5  " 0 Invalid_Index;
  test_parse_error "Invalid Mark Test 1" "mark 500 500  " 9 Invalid_Index;
  test_parse_error "Malformed Mark Test 1" "mark blah blah" 5 Malformed;
  test_parse_error "Malformed Mark Test 2" "mark     blah" 5 Malformed;
  test_parse_error "Malformed Quit Test" " quit now " 3 Malformed;
  test_parse_error "Malformed Help Test" " help   me " 8 Malformed;
  test_parse_error "Malformed Restart Test" " restart  i done " 2 Malformed;
  test_parse_error "Single String Malformed Test" " bleh " 3 Malformed;
  test_parse_error "Multiple String Malformed Test" " tic tac toe  " 8 Malformed
]

let monte_tests = [
  test_cartesian_product "Empty sets" [] [] [];
  test_cartesian_product "Left empty" [] [1; 3; 4] [];
  test_cartesian_product "Right empty" [3; 63; 69; 20] [] [];
  test_cartesian_product "One element sets" [4] [5] [(4, 5)];
  test_cartesian_product "Mixed1" [3; 63; 69; 20] [5]
    [(3, 5); (63, 5); (69, 5); (20, 5)];
  test_cartesian_product "Mixed2" [3; 63; 69; 20] [5; 1; 20; 9]
    [(3, 5); (3, 1); (3, 20); (3, 9); (63, 5); (63, 1); (63, 20); (63, 9);
     (69, 5); (69, 1); (69, 20); (69, 9); (20, 5); (20, 1); (20, 20); (20, 9)]
]

(* The [test_many_games] test runs 100 simulations of AI playing against AI, and
   takes a very long time. If this flag is turned off, that test is run with 10
   simulations instead. *)
let comprehensive = false

let game_tests = [
  let n = if comprehensive then 100 else 10 in
  test_many_games "Testing random vs monte AI" n game_state true
]

let suite = "search test suite" >:::
            List.concat [board_tests; command_tests; monte_tests; game_tests]

let _ = run_test_tt_main suite
