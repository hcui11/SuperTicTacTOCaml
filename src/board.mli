(** Governs the game board of an Ultimate Tic Tac TOCaml game. *)

(** The type of squares. *)
type square =
  | X
  | O
  | Blank

(** The type of a 3x3 Tic-Tac-Toe board. *)
type board = square array

(** The type of an Ultimate Tic-Tac-Toe game. *)
type game = board array 

(** The initialized sub-board full of blanks. *)
val blank_board : unit -> board

(** The initialized game full of blanks. *)
val initial_game : unit -> game

(** [index_of_coordinate x y] is the index of the coordinate [(x,y)] in a 
    game. *)
val index_of_coordinate : int -> int -> int

(** [board_of_coordinate x y] is the board that [(x, y)] is located in. *)
val board_of_coordinate : int -> int -> int

(** [set_game_square t x y curr input] sets the square at coordinate [(x,y)] in 
    a board at index [curr] in game [t] to the given input. *)
val set_game_square : game -> int -> int -> int -> square -> unit

(** [get_game_square t x y curr] gets the square at coordinate [(x,y)] in a 
    board at index [curr] in game [t]. *)
val get_game_square : game -> int -> int -> int -> square

(** [get_board gm i] gets the board at index [i] in game [gm]. *)
val get_board : game -> int -> board

(** [is_full bd] returns true if the board [bd] contain [Blank]. *)
val is_full : board -> bool

(** [set_board_square bd i in] sets the square at index [i] in a board [bd] to 
    value [in]*)
val set_board_square : board -> int -> square -> unit

(** [get_board_square bd i] gets the square at index [i] in a board [bd]. *)
val get_board_square : board -> int -> square 

(** [tie_breaker bd] breaks ties if [bd] is full and has no winner. Returns the
    square value with the majority on the board [bd].

    Requires: [is_full bd = true && check_win bd = false] *)
val tie_breaker : board -> square

(** [check_win bd] returns true if one player has won the board [bd]. *)
val check_win : board -> bool

(** [other_square sq] returns the opposite square of [sq]. 
    Requires: [sq] cannot be [Blank]. *)
val other_square : square -> square

(** [print_grid g curr mini] prints game grid [g] with currently selected board
    [curr] along with associated mini-board [mini] to the console. *)
val print_grid : game -> int -> board -> unit 
