(** Governs the game state. *)

(** The abstract type of values representing the game state. *)
type t

type stat = {name : string;
              mutable moves : float;
              mutable time : float}

(** Raised when attempting an invalid move. *)
exception Invalid_Move

(** The current game state. *)
val game_state : t

val get_stats : t -> stat list

(** [create_state t] is a copy of the current game state. *)
val create_state : t -> t

(** [reset_game t] restores the game [t] to its default values, essentially
    resetting the game. *)
val reset_game : t -> unit

(** [player_move x y t b] changes the game [t] to represent a player marking a
    coordinate [(x, y)] with [X] or [O]. [b] is false for the AI and true for
    player.
    Raises [Invalid_Move] if the player cannot mark coordinate [(x,y)]*)
val player_move : int -> int -> t -> bool -> unit

(** [curr_board t] is the index of the current board in the state of the game
    [t]. *)
val curr_board : t -> int

(** [curr_player t] is the current player making their move in the state of the
    game [t]. *)
val curr_player : t -> Board.square

(** [get_big_board t] is the current board in the state of the game [t]. *)
val get_big_board : t -> Board.game

(** [get_small_board t] is a smaller board that represents winners of sub-boards
    in the state of the game [t]. *)
val get_small_board : t -> Board.board

val initialize_stats : t -> string -> unit

val get_name : t -> string



