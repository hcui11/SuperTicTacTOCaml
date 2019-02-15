open Yojson.Basic.Util

type t

(** [score_type] specifies which highscore type will be used *)
type score_type = TIME | MOVES

(** [get_high_scores t len] returns the top [len] high_scores found in t. *)
val get_high_score : string -> State.t -> score_type -> float

(** [update_json s t state] updates the json file [s] with the current contents
    of [State.stat] in [state] based on [t] *)
val update_json : string -> score_type -> State.t -> unit

(** [print_score s f t] prints the first [size] elements of the given 
    json file [file] *)
val print_score : int -> string -> score_type -> unit
