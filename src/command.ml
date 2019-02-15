type command =
  | Restart
  | Mark of (int*int)
  | Help
  | Quit

exception Empty
exception Malformed
exception Invalid_Index

(** [check_coordinate (x,y) bd] is whether or not the coordinate [(x,y)] 
    represents a valid coordinate in the board at index [bd]. *)
let check_coordinate (x, y) bd =
  let low_x_bound = (bd mod 3) * 3 in
  let low_y_bound = (bd / 3) * 3 in
  let high_x_bound = low_x_bound + 3 in
  let high_y_bound = low_y_bound + 3 in
  if bd <> 9 then 
    low_x_bound <= x && x < high_x_bound && low_y_bound <= y && y < high_y_bound 
  else 
    0 <= x && x < 9 && 0 <= y && y < 9

(** [lst_to_coordinate bd lst] is a coordinate representation of a list of two
    numbers [lst] if [lst] represents a valid coordinate in the board at index 
    [bd]
    Raises [Invalid_Index] if the [lst] does not represent a coordinate in the
    board at index [bd].
    Raises [Malformed] if the [lst] does not represent a coordinate. *)
let lst_to_coordinate bd = function
  | sx :: sy :: [] -> 
    begin
      try 
        let x = int_of_string sx in
        let y = int_of_string sy in
        if check_coordinate (x,y) bd then (x,y) else raise Invalid_Index
      with Failure _ -> raise Malformed
    end
  | _ -> raise Malformed

let parse str bd =
  let str_list = String.split_on_char ' ' str in
  match List.filter (fun x -> x <> "") str_list with
  | [] -> raise Empty
  | x :: [] ->
    begin
      match x with
      | "restart" -> Restart
      | "help" -> Help
      | "quit" -> Quit
      | _ -> raise Malformed
    end
  | x :: xs ->
    if x = "mark" then Mark (lst_to_coordinate bd xs)
    else raise Malformed
