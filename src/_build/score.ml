open Yojson.Basic.Util
open State

type score_type = TIME | MOVES

type t = {name : string; value : float}

(** [score_to_string s] converts the score_type [s] to a string *)
let score_to_string = function
  | TIME -> "time"
  | MOVES -> "move"

(** [json_to_score j] converts the json object [j] to type t *)
let json_to_score json = {
  name = json |> member "name" |> to_string;
  value = json |> member "key" |> to_float }

(** [json_to_scor_list j t] converts the json object [j] to type t list
    based on the score_type [t] *)
let json_to_score_list json t = 
    json |> member (score_to_string t) |> to_list |> List.map json_to_score

(** [t_compare a b] returns a negative number when b > a,
    positive number when a > b, and 0 when a = b *)
let t_compare a b =
  match a.value -. b.value with
  | x when x = 0.0 -> 0
  | x when x < 0.0 -> -1
  | x -> 1

(** [sort_score_list s] sorts the elements of [s] in ascending order
 *)
let sort_score_list (scores : t list) = 
    List.sort_uniq t_compare scores

(** [t_to_string t] converts the type [t] to a string *)
let t_to_string t =
  print_string (t.name ^ " : " ^ (string_of_float t.value) ^ "\n")

(** [print_score s f t] prints the first [s] elements in the json file [f] based
    on the score_type [t]. Prints scores in an ascending order *)
let print_score size file t =
  let json = Yojson.Basic.from_file file in
  let score_list = json_to_score_list json t in
  let countdown = ref 0 in
  try
    while !countdown <> size do
      t_to_string (List.nth score_list !countdown);
      countdown := !countdown + 1;
    done;
  with Failure _ -> ()

(** [change_score s n v acc] searches through the list [s] attempting to find a 
    value with the same name as [n]. If the name is found, replace the current 
    value with [v] and return the list. If no value is matched, return the 
    same list *)
let rec change_score (scores : t list) name new_value acc =
  match scores with
  | x :: xs when name = x.name -> 
    (List.rev acc @ [{name = x.name; value = 
      (if new_value > x.value then x.value else new_value)}] @ xs, true)
  | x :: xs -> change_score xs name new_value (x :: acc)
  | [] -> (List.rev acc, false)

(** [add_score s n v] adds a new type t with [n] and [v] to [s] *)
let add_score scores name valu = 
  match change_score scores name valu [] with
  | (x, b) when b -> sort_score_list x
  | (x, b) -> sort_score_list ({name = name; value = valu} :: scores)

(** [get_high_score f s t] gets the score of the player in [s] from the json 
    file [file] based on score_type [t] *)
let get_high_score file state t =
  let json = Yojson.Basic.from_file file in
  let score_list = json_to_score_list json t in
  let rec score scores state =
    match scores with
    | x :: xs when x.name = (State.get_name state) -> x.value
    | x :: xs -> score xs state
    | [] -> 0.0
  in score score_list state

(** [stat_of_stats s p] creates a type t option based on the contents of 
    State.stat in [s] based on the score_type [p] *)
let stat_of_stats (state : State.t) play_t = 
  match (State.get_stats state) with
  | [] -> None
  | x :: xs -> if play_t = MOVES then
                  Some {name = x.name; value = x.moves}
                else
                  Some {name = x.name; value = x.time}

(** [get_stat s] converts the t option [s] to type t *)
let get_stat stat = 
  match stat with
  | Some x -> x
  | None -> {name = ""; value = 0.0}

(** [add_to_file f j] updates the json file [f] with the contents of [j] *)
let add_to_file filename json =
  let channel = open_out filename in
  Yojson.Basic.to_channel channel json;
  close_out channel

(** [assox_to_json_list s] converts the t list [s] to a list of json object*)
let assoc_to_json_list scores =
  let rec to_list scores acc =
  match scores with
    | {name;value} :: xs -> 
    to_list xs ((`Assoc [("name",`String name); ("key", `Float value)]) :: acc)
    | [] -> List.rev acc
  in to_list scores []

(** [assoc_list_to_json t s] converts the t list [s] to a json object based on 
    the score_type [s] *)
let assoc_list_to_json t scores = 
  `Assoc [((score_to_string t), `List (assoc_to_json_list scores))]

(** [update_json f t s] adds the current contents of [s] to the json file [f]
    based on the score_type [t]. New score is added to the json file [f] in a
    sorted order *)
let update_json filename t state =
  let json = Yojson.Basic.from_file filename in
  let current = json_to_score_list json t in
  let stats = get_stat (stat_of_stats state t) in
  let updated = if stats.name <> "" then
      add_score current stats.name stats.value
      else [] in
  if (updated <> []) then add_to_file filename (assoc_list_to_json t updated)
  else ();

