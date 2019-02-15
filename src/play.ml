open Board
open Command
open State
open UI_messages
open Sys
open Unix
open Score

type playmode = VSPlayer | VSComputer | Test
type sub_selection = Easy | Hard | Host | Join 
type style = Normal | Blitz

(** [max_socket_data_length] is the maximum allowed length of data transmitted
    socket communication. *)
let max_socket_data_length = 50

(** [timer] is the countdown timer loaded at the start of the turn. *)
let timer = ITIMER_REAL

let max_time = 180.

(** [time_left_X] is the amount of time player X has left. *)
let time_left_X = ref 180.

(** [time_left_O] is the amount of time player O has left. *)
let time_left_O = ref 180.

let set_time_stat stat =
  try
    let stats = List.hd stat in
    stats.time <- max_time -. !time_left_X
  with Failure _ -> ()

(** Prints an exit message and exits the program. *)
let quit_game () =
  let msg = "\nThanks for playing!\n" in
  ANSITerminal.(print_string [cyan] msg);
  exit 0

(** [set_time p] sets the countdown timer to the amount of time player [p] has
    left. *)
let set_time p = 
  if p = X 
  then setitimer timer {it_interval = 1.; it_value = !time_left_X}
  else setitimer timer {it_interval = 1.; it_value = !time_left_O}

(** [set_buffer_time ()] sets the countdown timer to a time buffer between 
    rounds.*)
let set_buffer_time () = 
  setitimer timer {it_interval = 1.; it_value = 3600.}

(** [update_time p] updates the amount of time player [p] has left. *)
let update_time p =
  if p = X
  then time_left_X := (getitimer timer).it_value
  else time_left_O := (getitimer timer).it_value

(** [handle _] prints the winning message and exits the game. *)
let handle _ = print_win_msg (curr_player game_state) false; quit_game()

(** [signal_handle ()] determines how to handle a signal once a player has no 
    time left. *)
let signal_handle f = set_signal sigalrm (Signal_handle f)

(** [reset_time ()] resets the player times for a new game. *)
let reset_time () = time_left_O := 180.; time_left_X := 180.

(** [perform_AI_move] executes a single AI move on game state [t]. The AI
    strategy is determined by the difficulty level [sub_select]. *)
let perform_AI_move (state : State.t) (sub_select: sub_selection) 
    (print_on : bool) =
  let (x, y) = 
    match sub_select with
    | Easy -> RandomAI.next_move state
    | Hard -> Monte.next_move state 
    | _ -> failwith "No difficulty provided" in

  let coordinate_str = string_of_int x ^ ", " ^ string_of_int y in
  if print_on then
    print_string ("\n\nAI Move: (" ^ coordinate_str ^ ")\n");
  player_move x y state false

(** [send_data sock data] sends [data] to socket [sock]. *)
let send_data sock data =
  let byte = Bytes.of_string data in
  let length = Bytes.length byte in
  if length > max_socket_data_length then
    failwith "Maximum buffer length exceeded"
  else
    ignore(send sock byte 0 length [])

(** [receive_data sock] is the data received from socket [sock]. Can have a
    maximum length of [max_socket_data_length]. *)
let receive_data sock =
  let container = Bytes.create max_socket_data_length in
  let received_length = recv sock container 0 max_socket_data_length [] in
  (Bytes.sub container 0 received_length) |> Bytes.to_string  

let rec execute 
    (state : State.t)
    (clnt : Unix.file_descr)
    (serv : Unix.file_descr)
    (sty : style)
    (mode : playmode)
    (sub_select : sub_selection)
    (print_on : bool) : Board.square =

  let handle_timeout n =
    let player = curr_player game_state in
    match sub_select, player with
    | Host, X -> 
      send_data clnt "Opponent Timeout!";
      print_win_msg (curr_player game_state) false;
      quit_game()
    | Join, O ->
      send_data serv "Opponent Timeout!";
      print_win_msg (curr_player game_state) false;
      quit_game()
    | _, _ -> (); in

  if mode = VSPlayer then
    signal_handle handle_timeout
  else
    signal_handle handle;

  let small_board = get_small_board state in

  if print_on then
    begin
      ignore(Sys.command "clear");
      print_grid (get_big_board state) (curr_board state) (small_board);
    end;
  if not (is_full small_board) then
    begin
      match check_win small_board with
      | false ->
        begin
          match sty, mode, (curr_player state) with
          (* In our AI vs AI Test, this pattern matches the Hard AI's turn. *)
          | _, Test, X ->
            perform_AI_move state Hard false;
            execute state clnt serv sty Test Hard print_on;
            (* In our AI vs AI Test, this pattern matches the Easy AI's turn. *)
          | _, Test, O ->
            perform_AI_move state Easy false;
            execute state clnt serv sty Test Easy print_on;
            (* In our Normal AI vs Player Mode, this pattern matches the AI's 
               turn. *)
          | Normal, VSComputer, O -> 
            perform_AI_move state sub_select true;
            execute state clnt serv sty mode sub_select print_on;
            (* In our Blitz AI vs Player Mode, this pattern matches the AI's 
               turn. *)
          | Blitz, VSComputer, O ->
            ignore (set_time O);
            perform_AI_move state sub_select true;
            update_time O;
            execute state clnt serv sty mode sub_select print_on;
            (* In our Normal AI vs Player Mode, this pattern matches the
               Player's turn *)
          | Normal, VSComputer, _ ->
            begin
              prompt_for_coordinates (curr_player state);
              let line = read_line() in
              begin
                match parse line (curr_board state) with
                | exception Invalid_Index ->
                  print_invalid_index_error_msg (); 
                  execute state clnt serv sty mode sub_select print_on;
                | exception Empty ->
                  print_empty_error_msg ();
                  execute state clnt serv sty mode sub_select print_on;
                | exception Malformed ->
                  print_malformed_error_msg ();
                  execute state clnt serv sty mode sub_select print_on;
                | Restart ->
                  reset_game state;          
                  execute state clnt serv sty mode sub_select print_on;
                | Help ->
                  print_help ();
                  execute state clnt serv sty mode sub_select print_on;
                | Mark (x, y) ->
                  begin
                    try player_move x y state true; 
                      execute state clnt serv sty mode sub_select print_on;
                    with Invalid_Move ->
                      print_invalid_index_error_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                  end
                | Quit -> quit_game()
              end
            end
          (* In our Blitz AI vs Player Mode, this pattern matches the
             Player's turn. *)
          | Blitz, VSComputer, _ ->
            begin 
              let player = curr_player state in
              ignore (set_time player);
              let time = int_of_float (getitimer timer).it_value in
              let m = string_of_int (time / 60) in
              let s0 = string_of_int (time mod 10) in
              let s1 = string_of_int ((time mod 60) / 10) in
              print_string ("Your Time Remaining: [" ^ m ^ ":" ^ s1 ^ s0 ^ "]");
              prompt_for_coordinates (curr_player state);
              let line = read_line() in
              begin
                match parse line (curr_board state) with
                | exception Invalid_Index ->
                  print_invalid_index_error_msg (); 
                  update_time player;
                  execute state clnt serv sty mode sub_select print_on;
                | exception Empty ->
                  print_empty_error_msg ();
                  update_time player;
                  execute state clnt serv sty mode sub_select print_on;
                | exception Malformed ->
                  print_malformed_error_msg ();
                  update_time player;
                  execute state clnt serv sty mode sub_select print_on;
                | Restart ->
                  reset_game state;
                  reset_time ();          
                  execute state clnt serv sty mode sub_select print_on;
                | Help ->
                  print_help ();
                  update_time player;
                  execute state clnt serv sty mode sub_select print_on;
                | Mark (x, y) ->
                  begin
                    try player_move x y state true; 
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    with Invalid_Move ->
                      print_string "player \n";
                      print_invalid_index_error_msg ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                  end
                | Quit -> quit_game();
              end
            end
          (* In our Normal Player vs Player Mode, this pattern matches the
             Player X's turn. *)
          | Normal, VSPlayer, X ->
            begin
              match sub_select with
              | Host ->
                begin
                  prompt_for_coordinates (X);
                  let input_command = read_line() in
                  begin
                    match parse input_command (curr_board state) with
                    | exception Invalid_Index ->
                      print_invalid_index_error_msg (); 
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Empty ->
                      print_empty_error_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Malformed ->
                      print_malformed_error_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Restart ->
                      print_reset_disabled_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Help ->
                      print_help ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Mark (x, y) ->
                      begin
                        try player_move x y state true;
                          send_data clnt input_command;
                          execute state clnt serv sty mode sub_select print_on;
                        with Invalid_Move ->
                          print_string "player \n";
                          print_invalid_index_error_msg ();
                          execute state clnt serv sty mode sub_select print_on;
                      end
                    | Quit ->
                      send_data clnt input_command;
                      quit_game()
                  end
                end
              | Join ->
                begin
                  print_endline "Waiting for opponent's move...";
                  print_endline "Here is the current board:\n";
                  print_grid (get_big_board state) (curr_board state)
                    (small_board);
                  let opponent_command = receive_data serv in
                  match parse opponent_command (curr_board state) with
                  | exception Malformed ->
                    print_timeout_msg ();
                    exit 0
                  | Mark (x, y) ->
                    begin
                      try player_move x y state true;
                        execute state clnt serv sty mode sub_select print_on;
                      with Invalid_Move ->
                        print_string "player \n";
                        print_invalid_index_error_msg ();
                        execute state clnt serv sty mode sub_select print_on;
                    end
                  | Quit ->
                    print_endline "Your opponent quit.";
                    quit_game()
                  | _ -> failwith "impossible"
                end
              | _ -> failwith "No host/client setting given"
            end
          (* In our Normal Player vs Player Mode, this pattern matches the
             Player O's turn. *)
          | Normal, VSPlayer, O ->
            begin
              match sub_select with
              | Host ->
                begin
                  print_endline "Waiting for opponent's move...";
                  let opponent_command = receive_data clnt in
                  match parse opponent_command (curr_board state) with
                  | exception Malformed ->
                    print_timeout_msg ();
                    exit 0
                  | Mark (x, y) ->
                    begin
                      try player_move x y state true;
                        execute state clnt serv sty mode sub_select print_on;
                      with Invalid_Move ->
                        print_string "player \n";
                        print_invalid_index_error_msg ();
                        execute state clnt serv sty mode sub_select print_on;
                    end
                  | Quit ->
                    print_endline "Your opponent quit.";
                    quit_game()
                  | _ -> failwith "impossible"
                end
              | Join ->
                begin
                  prompt_for_coordinates (O);
                  let input_command = read_line() in
                  begin
                    match parse input_command (curr_board state) with
                    | exception Invalid_Index ->
                      print_invalid_index_error_msg (); 
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Empty ->
                      print_empty_error_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Malformed ->
                      print_malformed_error_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Restart ->
                      print_reset_disabled_msg ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Help ->
                      print_help ();
                      execute state clnt serv sty mode sub_select print_on;
                    | Mark (x, y) ->
                      begin
                        try player_move x y state true;
                          send_data serv input_command;
                          execute state clnt serv sty mode sub_select print_on;
                        with Invalid_Move ->
                          print_string "player \n";
                          print_invalid_index_error_msg ();
                          execute state clnt serv sty mode sub_select print_on;
                      end
                    | Quit ->
                      send_data serv input_command;
                      quit_game()
                  end
                end
              | _ -> failwith "No host/client setting given"
            end
          (* In our Blitz Player vs Player Mode, this pattern matches the
             Player X's turn. *)
          | Blitz, VSPlayer, X ->
            begin
              match sub_select with
              | Host ->
                begin
                  let player = curr_player state in
                  ignore (set_time player);
                  let time = int_of_float (getitimer timer).it_value in
                  let m = string_of_int (time / 60) in
                  let s0 = string_of_int (time mod 10) in
                  let s1 = string_of_int ((time mod 60) / 10) in
                  print_string ("Your Time Remaining: ["
                                ^ m ^ ":" ^ s1 ^ s0 ^ "]");
                  prompt_for_coordinates (X);
                  let input_command = read_line() in
                  begin
                    match parse input_command (curr_board state) with
                    | exception Invalid_Index ->
                      print_invalid_index_error_msg (); 
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Empty ->
                      print_empty_error_msg ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Malformed ->
                      print_malformed_error_msg ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | Restart ->
                      print_reset_disabled_msg ();  
                      update_time player;       
                      execute state clnt serv sty mode sub_select print_on;
                    | Help ->
                      print_help ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | Mark (x, y) ->
                      begin
                        try player_move x y state true;
                          send_data clnt input_command;
                          update_time player;
                          execute state clnt serv sty mode sub_select print_on;
                        with Invalid_Move ->
                          print_string "player \n";
                          print_invalid_index_error_msg ();
                          update_time player;
                          execute state clnt serv sty mode sub_select print_on;
                      end
                    | Quit ->
                      send_data clnt input_command;
                      quit_game()
                  end
                end
              | Join ->
                begin
                  ignore(set_buffer_time ());
                  print_endline "Waiting for opponent's move...";
                  print_endline "Here is the current board:\n";
                  print_grid (get_big_board state) (curr_board state)
                    (small_board);
                  let opponent_command = receive_data serv in
                  match parse opponent_command (curr_board state) with
                  | exception Malformed ->
                    print_timeout_msg ();
                    exit 0
                  | Mark (x, y) ->
                    begin
                      try player_move x y state true;
                        execute state clnt serv sty mode sub_select print_on;
                      with Invalid_Move ->
                        print_string "player \n";
                        print_invalid_index_error_msg ();
                        execute state clnt serv sty mode sub_select print_on;
                    end
                  | Quit ->
                    print_endline "Your opponent quit.";
                    quit_game()
                  | _ -> failwith "impossible"
                end
              | _ -> failwith "No host/client setting given"
            end
          (* In our Blitz Player vs Player Mode, this pattern matches the
             Player O's turn. *)
          | Blitz, VSPlayer, O ->
            begin
              match sub_select with
              | Host ->
                begin
                  ignore(set_buffer_time ());
                  print_endline "Waiting for opponent's move...";
                  print_endline "Here is the current board:\n";
                  print_grid (get_big_board state) (curr_board state)
                    (small_board);
                  let opponent_command = receive_data clnt in
                  match parse opponent_command (curr_board state) with
                  | exception Malformed ->
                    print_timeout_msg ();
                    exit 0
                  | Mark (x, y) ->
                    begin
                      try player_move x y state true;
                        execute state clnt serv sty mode sub_select print_on;
                      with Invalid_Move ->
                        print_string "player \n";
                        print_invalid_index_error_msg ();
                        execute state clnt serv sty mode sub_select print_on;
                    end
                  | Quit ->
                    print_endline "Your opponent quit.";
                    quit_game()
                  | _ -> failwith "impossible"
                end
              | Join ->
                begin
                  let player = curr_player state in
                  ignore (set_time player);
                  let time = int_of_float (getitimer timer).it_value in
                  let m = string_of_int (time / 60) in
                  let s0 = string_of_int (time mod 10) in
                  let s1 = string_of_int ((time mod 60) / 10) in
                  print_string ("Your Time Remaining: ["
                                ^ m ^ ":" ^ s1 ^ s0 ^ "]");
                  prompt_for_coordinates (O);
                  let input_command = read_line() in
                  begin
                    match parse input_command (curr_board state) with
                    | exception Invalid_Index ->
                      print_invalid_index_error_msg (); 
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Empty ->
                      print_empty_error_msg ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | exception Malformed ->
                      print_malformed_error_msg ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | Restart ->
                      print_reset_disabled_msg (); 
                      update_time player;          
                      execute state clnt serv sty mode sub_select print_on;
                    | Help ->
                      print_help ();
                      update_time player;
                      execute state clnt serv sty mode sub_select print_on;
                    | Mark (x, y) ->
                      begin
                        try player_move x y state true;
                          send_data serv input_command;
                          update_time player;
                          execute state clnt serv sty mode sub_select print_on;
                        with Invalid_Move ->
                          print_string "player \n";
                          print_invalid_index_error_msg ();
                          update_time player;
                          execute state clnt serv sty mode sub_select print_on;
                      end
                    | Quit ->
                      send_data serv input_command;
                      quit_game()
                  end
                end
              | _ -> X
            end
          | _, _, _ -> failwith "impossible"
        end
      | true -> 
        begin
          let prev_player = curr_player state in
          print_win_msg (prev_player) false;
          set_time_stat (get_stats state);
          if ( mode = VSComputer && prev_player = O) then
            if(sty = Normal) then begin
              update_json "moves.json" MOVES state;
              print_highscore "moves.json" MOVES state
            end
            else
              begin
                update_json "time.json" TIME state;
                print_highscore "time.json" TIME state
              end;
          other_square (curr_player state);
        end
    end

  else
  if check_win small_board then
    begin
      let prev_player = curr_player state in
      print_win_msg (curr_player state) false;
      set_time_stat (get_stats state);
      if(mode = VSComputer && prev_player = O) then
        if(sty = Normal) then begin
          update_json "moves.json" MOVES state;
          print_highscore "moves.json" MOVES state
        end
        else
          begin
            update_json "time.json" TIME state;
            print_highscore "time.json" TIME state
          end; 
      other_square (curr_player state);
    end
  else
    begin
      let cur_player = tie_breaker small_board in
      print_win_msg (cur_player) true;
      set_time_stat (get_stats state);
      if(mode = VSComputer && cur_player = X) then
        if(sty = Normal) then begin
          update_json "moves.json" MOVES state;
          print_highscore "moves.json" MOVES state
        end
        else
          begin
            update_json "time.json" TIME state;
            print_highscore "time.json" TIME state
          end; 
      curr_player state;
    end
