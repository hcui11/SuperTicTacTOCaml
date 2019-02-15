(** Parses and handles player commands. *)

(** The type of player inputted commands. *)
type command =
  | Restart
  | Mark of (int*int)
  | Help
  | Quit

(** Raised when the player inputs an empty command. *)
exception Empty

(** Raised when the player inputs an invalid command. *)
exception Malformed

(** Raised when the player attempts to mark an invalid coordinate. *)
exception Invalid_Index

(** Parses a string into a command by stripping out spaces. Raises exception
    [Empty] if an empty string is parsed or [Malformed] if an invalid command
    is parsed. *)
val parse : string -> int -> command