(** An AI that moves by choosing an available square at random. Used for the
    easy difficulty. *)

(** [next_move ()] is the next random move for the AI. *)
val next_move : State.t -> int * int