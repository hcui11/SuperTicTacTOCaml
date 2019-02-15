(** Printed messages for the user interface. *)

open Board

(** Prints a welcome message (with welcome art). *)
val print_welcome_msg : unit -> unit

(** Prints the scoreboard. *)
val print_scoreboard : unit -> unit

(** [curr_player_str s] returns a string representation of a player [s]. *)
val curr_player_str : square -> string

(** Prints a prompt message for the user to enter coordinates to mark. *)
val prompt_for_coordinates : square -> unit

(** Prints a help message. *)
val print_help : unit -> unit

(** Prints an error message for invalid coordinates. *)
val print_invalid_index_error_msg : unit -> unit

(** Prints an error message for an empty player input. *)
val print_empty_error_msg : unit -> unit

(** Prints an error message for a malformed command. *)
val print_malformed_error_msg : unit -> unit

(** Prints an error message for a player attempting to reset the board in a
    networked game. *)
val print_reset_disabled_msg : unit -> unit

(** Prints an error message for a failed connection to the given IP address. *)
val print_connection_failed_msg : unit -> unit

(** Prints a win message for the player [p]. *)
val print_win_msg : square -> bool -> unit

(** [print_highscore f s t] prints the highest score of the current player in
    [t] obtained from the file [f]. [s] signifies the score type *)
val print_highscore : string -> Score.score_type -> State.t -> unit

(** Prints a win message if the opponent times out. *)
val print_timeout_msg : unit -> unit
