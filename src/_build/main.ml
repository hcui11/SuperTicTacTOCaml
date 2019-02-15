open State
open Play
open Unix
open UI_messages

(** [ideal_width] is the smallest terminal window width at which a user can
    play comfortably. *)
let ideal_width = 108
(** [ideal_height] is the smallest terminal window height at which a user can
    play comfortably. *)
let ideal_height = 38

(** If a user has window dimensions smaller than [ideal_width * ideal_height],
    adjusts terminal dimensions to match the ideal dimensions. *)
let resize_window () =
  let w = fst (ANSITerminal.size()) in
  let h = snd (ANSITerminal.size()) in
  if w < ideal_width || h < ideal_height then
    ANSITerminal.resize ideal_width ideal_height

(** Prints an exit message and exits the program. *)
let quit_game () =
  let msg = "\nThanks for playing!\n" in
  ANSITerminal.(print_string [cyan] msg);
  exit 0

(** Prompts the user for whether they want to host a game or join a game. Only
    called if the user chose to play against another player. *)
let rec prompt_status () =
  let msg = "\nEnter \"host\" to host a game or \"join\" to join a game.\n> " in
  print_string msg;
  match read_line() with
  | "host" -> Host
  | "join" -> Join
  | "quit" -> quit_game()
  | _ -> prompt_status()

(** Prompts the user for a difficulty setting. Only called if the user chose to
    play against the computer. *)
let rec prompt_difficulty () =
  let msg = "\nEnter your desired difficulty: \"easy\" or \"hard\".\n> " in
  print_string msg;
  match read_line() with
  | "easy" -> Easy
  | "hard" -> Hard
  | "quit" -> quit_game()
  | _ -> prompt_difficulty()

(** Prompts the user for a playing mode. *)
let rec prompt_mode () =
  let msg = "\nEnter \"vsplayer\" to play against a friend, or \"vscomputer\"" ^
            " to play against the computer.\n> " in
  print_string msg;
  match read_line() with
  | "vsplayer" -> VSPlayer, prompt_status()
  | "vscomputer" -> VSComputer, prompt_difficulty()
  | "quit" -> quit_game()
  | _ -> prompt_mode()

(** Prompts the user for a name. *)
let prompt_player_name () =
  let msg = "\nWhat is your name?\n> " in
  print_string msg;
  read_line ()

(** Creates a player for the score board from the player name. *)
let make_player mode =
  match mode with
  | x when x = VSComputer -> let f = prompt_player_name () in
    f
  | x -> ""

(** Prompts the user for the style of game. *)
let rec prompt_style () =
  let msg = "\nEnter \"normal\" to play a normal mode, or \"blitz\" to play " ^ 
            "blitz mode. \n> " in 
  print_string msg;
  match read_line() with
  | "normal" -> Normal, prompt_mode()
  | "blitz" -> Blitz, prompt_mode()
  | "quit" -> quit_game()
  | _ -> prompt_style ()

(** Prompts the user for an IP address. *)
let rec prompt_address () = 
  let msg = "\nEnter a valid IP address of the host. \n> " in
  print_string msg;
  match read_line () with
  | "quit" -> exit 0
  | address ->
    begin
      try 
        inet_addr_of_string address
      with _ -> prompt_address ()
    end

(** Prompts the user to start the game. *)
let rec prompt_start () =
  let prompt_msg = "\nEnter \"start\" when you are ready to play, or \"scores\""
                    ^ " to view highscores.\n> " in
  print_string prompt_msg;
  match read_line () with
  | "start" ->
    let tuple = prompt_style() in
    let style = fst (tuple) in
    let mode = fst (snd tuple) in
    let diff = snd (snd tuple) in
    let player = make_player mode in
    if player <> "" then initialize_stats game_state player;
    ignore(Sys.command "clear");
    begin
      let serv = socket PF_INET SOCK_STREAM 0 in 
      setsockopt serv SO_REUSEADDR true;
      match diff with
      | Host -> 
        let addr = prompt_address () in 
        begin 
          try
            bind serv (ADDR_INET (addr, 8888));
            listen serv 1;
            print_endline "\nWaiting for client...";
            let clnt, _ =  accept serv in
            print_endline "Connected!";
            ignore (execute game_state clnt serv style mode diff true)
          with _ ->
            print_connection_failed_msg ();
            exit 0
        end
      | Join ->
        let addr = prompt_address () in 
        begin
          try 
            connect serv (ADDR_INET(addr, 8888));
            ignore (execute game_state serv serv style mode diff true)
          with _ ->
            print_connection_failed_msg ();
            exit 0
        end
      | _ -> ignore (execute game_state serv serv style mode diff true)
    end
  | "scores" ->
      print_scoreboard ();
      prompt_start ();
  | "quit" -> quit_game()
  | _ -> prompt_start ()

(** Launches the main menu. Credit to http://patorjk.com/software/taag for being
    an inspiration for the text graphics. *)
let main () =
  ignore(Sys.command("clear"));
  resize_window();
  print_welcome_msg ();
  prompt_start ()


(* Start the game. *)
let () = main ()
