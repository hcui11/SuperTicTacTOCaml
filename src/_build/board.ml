open ANSITerminal

type square =
  | X
  | O
  | Blank

let other_square = function
  | X -> O
  | O -> X
  | _ -> failwith "Not possible"

type board = square array

type game = board array

(** The side length of the board. *)
let board_side_length = 3

(** The number of squares in a single sub-board, equal to 
    [board_side_length * board_side_length]. *)
let num = board_side_length * board_side_length

let blank_board () =
  Array.make num Blank

let initial_game () =
  [|blank_board (); blank_board (); blank_board (); 
    blank_board (); blank_board (); blank_board (); 
    blank_board (); blank_board (); blank_board ();|]

let index_of_coordinate x y =
  (y mod 3) * 3 + (x mod 3)

let board_of_coordinate col row =
  assert (col >= 0 && col < 9 && row >= 0 && row < 9);
  (row / 3) * 3 + (col / 3)

let set_game_square state x y curr input =
  let square_index = index_of_coordinate x y in
  Array.set state.(curr) square_index input

let get_game_square state x y curr =
  let square_index = index_of_coordinate x y in
  Array.get state.(curr) square_index

let get_board = Array.get

let is_full bd = Array.for_all (fun x -> x <> Blank) bd

let set_board_square bd index input = bd.(index) <- input

let get_board_square bd index = bd.(index)

let check_win bd =
  let w = Array.make 8 true in
  for x = 1 to Array.length bd - 1 do
    match x with
    | 1 when w.(0) -> w.(0) <- bd.(1) <> Blank && bd.(0) = bd.(1)
    | 2 when w.(0) -> w.(0) <- bd.(2) <> Blank && bd.(0) = bd.(2)
    | 3 when w.(1) -> w.(1) <- bd.(3) <> Blank && bd.(0) = bd.(3)
    | 4 when w.(2) || w.(3) || w.(4) || w.(6) ->
      begin
        w.(2) <- bd.(4) <> Blank && w.(2) && bd.(0) = bd.(4);
        w.(3) <- bd.(4) <> Blank && w.(3) && bd.(1) = bd.(4);
        w.(4) <- bd.(4) <> Blank && w.(4) && bd.(2) = bd.(4);
        w.(6) <- bd.(4) <> Blank && w.(6) && bd.(3) = bd.(4);
      end
    | 5 when w.(5) || w.(6) ->
      begin
        w.(5) <- bd.(5) <> Blank && w.(5) && bd.(2) = bd.(5);
        w.(6) <- bd.(5) <> Blank && w.(6) && bd.(3) = bd.(5);
      end
    | 6 when w.(1) || w.(4) ->
      begin
        w.(1) <- bd.(6) <> Blank && w.(1) && bd.(0) = bd.(6);
        w.(4) <- bd.(6) <> Blank && w.(4) && bd.(2) = bd.(6);
      end
    | 7 when w.(3) || w.(7) ->
      begin
        w.(3) <- bd.(7) <> Blank && w.(3) && bd.(1) = bd.(7);
        w.(7) <- bd.(7) <> Blank && w.(7) && bd.(6) = bd.(7);
      end
    | 8 when w.(2) || w.(5) || w.(7) ->
      begin
        w.(2) <- bd.(8) <> Blank && w.(2) && bd.(0) = bd.(8);
        w.(5) <- bd.(8) <> Blank && w.(5) && bd.(2) = bd.(8);
        w.(7) <- bd.(8) <> Blank && w.(7) && bd.(6) = bd.(8);
      end
    | _ -> ()
  done;
  Array.exists (fun a -> a) w

let tie_breaker bd =
  assert (not (check_win bd) && is_full bd);
  if Array.fold_left (fun acc x -> if x = X then acc + 1 else acc) 0 bd >= 5
  then X else O

(** [string_of_square s] is a string representation of a square [s]. *)
let string_of_square sq =
  match sq with
  | X -> "X"
  | O -> "O"
  | Blank -> " "

(** The default printed text style. *)
let def = [default]

(** The highlighted printed text style. *)
let sty = [green]

(** [print_column_labels] prints the column indices for the grid. *)
let print_column_labels () =
  print_string [default] "    ";
  for i = 0 to (num - 1) do
    print_string [default] ((string_of_int i) ^ "   ")
  done;
  print_string [default] "\n"

(** [print_horizontal_div h] prints the horizontal line between grid rows. *)
let print_horizontal_div hstyle =
  print_string hstyle "  ";
  print_string hstyle "+";
  for i = 0 to (num - 1) do
    print_string hstyle "---";
    let s = if i mod board_side_length = 2 && i <> 8 then sty else hstyle in
    print_string s "+"
  done

(** Prints a small board divider. *)
let print_mini_div () =
  print_string def "+---+---+---+\n"

(** Prints one row of the miniboard [bd]. *)
let mini_board_string (bd : board) : string array =
  let row_string n =
    let slice = List.map string_of_square [bd.(n); bd.(n + 3); bd.(n + 6)] in
    let concat_with_bar s1 s2 = s1 ^ " | " ^ s2 in
    (List.fold_left (concat_with_bar) "" slice) ^ " |" in
  Array.init 3 row_string

let print_grid g curr miniboard =
  print_string def "\n";
  print_column_labels ();
  print_horizontal_div def;
  print_string def "\n";
  for i = 0 to (num - 1) do
    print_string [default] ((string_of_int i) ^ " |");
    for j = 0 to (num - 1) do
      let curr_board = board_of_coordinate i j in
      let curr_index = index_of_coordinate i j in
      let sq_str = string_of_square g.(curr_board).(curr_index) in
      let sq_style = if curr_board = curr then (Inverse) :: def else def in
      print_string sq_style (" " ^ sq_str ^ " ");
      let vsty = if j mod board_side_length = 2 && j <> 8 then sty else def in
      print_string vsty "|"
    done;

    (* highlights sub-board boundaries in green *)
    let hsty = if i mod board_side_length = 2 && i <> 8 then sty else def in
    match i with
    | 2 ->
      print_string sty "           MINIMAP\n";
      print_horizontal_div hsty;
      print_string def "        ";
      print_mini_div ()
    | 3 | 4 | 5 as x ->
      let mini = mini_board_string miniboard in
      print_string def ("       " ^ mini.(x - 3) ^ "\n");
      print_horizontal_div hsty;
      print_string def "        ";
      print_mini_div ()
    | _ ->
      print_string def "\n";
      print_horizontal_div hsty;
      print_string def "\n"
  done
