(** An implementation of the Monte Carlo tree search algorithm, used for the
    hard difficulty AI. *)

(** [cartesian_product l1 l2] is a list of coordinates generated from the 
    cartesian product of two lists of [l1] and [l2]. *)
val cartesian_product : int list -> int list -> (int * int) list

(** [next_move ()] is the next optimal move for the AI. *)
val next_move : State.t -> int * int