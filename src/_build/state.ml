open Board

type stat = {name : string;
             mutable moves : float ;
             mutable time : float }

type t = {mutable big_board : game; 
          mutable small_board : board;
          mutable full : bool array;
          mutable curr : int;
          mutable player : square;
          mutable stats : stat list}

exception Invalid_Move

let get_stats t = t.stats

let get_name t = 
  match t.stats with
  | x :: xs -> x.name
  | [] -> ""

let get_big_board t = t.big_board

let get_small_board t = t.small_board

let curr_board t = t.curr

let curr_player t = t.player

let create_stats name = {name = name; moves = 0.0; time = 0.0}

let game_state = {big_board = initial_game ();
                  small_board = blank_board ();
                  full = Array.make 9 false;
                  curr = 9;
                  player = X;
                  stats = []}

let create_state gm = {big_board = Array.map Array.copy gm.big_board;
                       small_board = Array.copy gm.small_board;
                       full = Array.copy gm.full;
                       curr = gm.curr;
                       player = gm.player;
                       stats = gm.stats}

let reset_game gm =
  gm.big_board <- initial_game ();
  gm.small_board <- blank_board ();
  gm.full <- Array.make 9 false;
  gm.curr <- 9;
  gm.player <- X

let increment_move stat = 
  let stats = List.hd stat in
  stats.moves <- (stats.moves +. 1.0)

let initialize_stats gm name = 
  gm.stats <- [create_stats name]

let player_move x y gm inc =
  let curr = gm.curr in
  let next = index_of_coordinate x y in
  let bd_index = board_of_coordinate x y in
  let sq_value = get_game_square gm.big_board x y bd_index in

  match sq_value, curr with
  | Blank, 9 -> 
    begin
      (* Change square in big board *)
      set_game_square gm.big_board x y bd_index gm.player; 

      if gm.player = X && gm.stats <> [] && inc then (increment_move gm.stats)
      else ();

      (* Check for a winner in the subboard *)
      if get_board_square gm.small_board bd_index = Blank
      && check_win  (get_board gm.big_board bd_index)
      then 
        set_board_square gm.small_board bd_index gm.player
      else 
        ();

      (* Check for whether the subboard is full *)
      if is_full (get_board gm.big_board bd_index)
      then 
        begin
          gm.full.(bd_index) <- true;
          if get_board_square gm.small_board bd_index = Blank
          then
            set_board_square gm.small_board bd_index 
              (tie_breaker (get_board gm.big_board bd_index))
          else 
            ()
        end
      else 
        (); 

      (* Change the subboard for the next turn *)
      if gm.full.(next) then gm.curr <- 9 else gm.curr <- next;

      (* Change the player for the next turn *)
      gm.player <- if gm.player = X then O else X
    end
  | Blank, index -> 
    begin
      (* Change square in big board *)
      if index <> bd_index 
      then 
        raise Invalid_Move 
      else 
        set_game_square gm.big_board x y index gm.player;

      if gm.player = X && gm.stats <> [] && inc then (increment_move gm.stats)
      else ();

      (* Check for a winner in the subboard *)
      if get_board_square gm.small_board index = Blank &&
         check_win (get_board gm.big_board index)
      then
        set_board_square gm.small_board index gm.player
      else
        ();

      (* Check for whether the subboard is full *)
      if is_full (get_board gm.big_board index) 
      then 
        begin
          gm.full.(index) <- true;
          if get_board_square gm.small_board index = Blank
          then
            set_board_square gm.small_board index 
              (tie_breaker (get_board gm.big_board index))
          else 
            ()
        end
      else 
        (); 

      (* Change the subboard for the next turn *)
      if gm.full.(next) then gm.curr <- 9 else gm.curr <- next;

      (* Change the player for the next turn *)
      gm.player <- if gm.player = X then O else X
    end
  | _ -> raise Invalid_Move
