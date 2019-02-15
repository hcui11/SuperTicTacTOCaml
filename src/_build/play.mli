(** Runs the game logic. *)

(** The type of game modes. *)
type playmode = VSPlayer | VSComputer | Test

(** The type of AI difficulty or the client-server status of the player. *)
type sub_selection = Easy | Hard | Host | Join

(** The type of game style. *)
type style = Normal | Blitz

(** [execute st m diff print] executes player commands on game state [st] with
    game mode [m]. If [m = VSComputer], [diff] represents the difficulty of the
    computer AI. Prints the board grid after every move if [print] is [true]. *)
val execute : State.t -> Unix.file_descr -> Unix.file_descr -> style -> playmode 
                      -> sub_selection -> bool -> Board.square
