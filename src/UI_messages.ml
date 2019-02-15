open Board
open Score

let print_direction = ANSITerminal.(print_string [cyan])

let print_welcome_msg () =
  let welcomemsg =
    "\n
                        __          __  _                             _        
                        \\ \\        / / | |                           | |       
                         \\ \\  /\\  / /__| | ___ ___  _ __ ___   ___   | |_ ___  
                          \\ \\/  \\/ / _ \\ |/ __/ _ \\| '_ ` _ \\ / _ \\  | __/ _ \\ 
                           \\  /\\  /  __/ | (_| (_) | | | | | |  __/  |  █ (_) |
                            \\/  \\/ \___|_|\\___\\___/|_| |_| |_|\\___|   \\__\\___/ " in

  let graphicmsg =
    "\n\n\n
                      █                                                             █
                      █  ██╗  ██╗██╗  ██████╗██╗███╗   ███╗ █████╗ ██████╗███████╗  █
                      █  ██║  ██║██║  ╚═██╔═╝██║████╗ ████║██╔══██╗╚═██╔═╝██╔════╝  █
                      █  ██║  ██║██║    ██║  ██║██╔████╔██║███████║  ██║  █████╗    █
                      █  ██║  ██║██║    ██║  ██║██║╚██╔╝██║██╔══██║  ██║  ██╔══╝    █
                      █  ╚█████╔╝██████╗██║  ██║██║ ╚═╝ ██║██║  ██║  ██║  ███████╗  █
                      █   ╚════╝ ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝  ╚═╝  ╚══════╝  █
  ■■■■■■■■■■■■■■■■■■■■█■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■█■■■■■■■■■■■■■■■■■■■■
                      █              _                                              █
                      █         _   (_)                      _                      █
                      █        | |_  _  ___                 | |_  __ _  ___         █
                      █        |  _|| |/ __|                |  _ / _` |/ __|        █
                      █        | |_ | | (__                 | | | (_| | (__         █
                      █         \__||_|\___|                 \__ \__,_|\___|        █
                      █                                                             █
                      █                                                             █
  ■■■■■■■■■■■■■■■■■■■■█■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■█■■■■■■■■■■■■■■■■■■■■
                      █                                                             █
                      █    ████████╗ ██████╗  ██████╗ █████╗ ███╗   ███╗██╗         █
                      █    ╚══██╔══╝██╔═══██╗██╔════╝██╔══██╗████╗ ████║██║         █
                      █       ██║   ██║   ██║██║     ███████║██╔████╔██║██║         █
                      █       ██║   ██║   ██║██║     ██╔══██║██║╚██╔╝██║██║         █
                      █       ██║   ╚██████╔╝╚██████╗██║  ██║██║ ╚═╝ ██║███████╗    █
                      █       ╚═╝    ╚═════╝  ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝    █       \n\n\n\n" in

  print_direction (welcomemsg ^ graphicmsg)

let print_scoreboard () =
  print_string "BLITZ HIGHSCORES\n";
  print_string "NAME | SCORE\n";
  Score.print_score 10 "time.json" TIME;
  print_string "\nNORMAL HIGHSCORES\n";
  print_string "NAME | SCORE\n";
  Score.print_score 10 "moves.json" MOVES

let curr_player_str s =
  match s with
  | X -> "X"
  | O -> "O"
  | Blank -> failwith "Failure: curr_player returned Blank"

let prompt_for_coordinates s =
  let curr = curr_player_str s in
  let msg = "\n" ^ curr ^ ", please enter the coordinates of your move:\n" in
  ANSITerminal.(print_string [cyan] msg);
  print_string "> "

let print_help () =
  let help_strs = [
    "mark [r c] : marks the square at row [r] and column [c] as the current " ^ 
    "player.";
    "restart : resets the game.";
    "help : prints this message.";
    "quit : quits the game.\n"
  ] in
  ANSITerminal.(print_string [green] (String.concat "\n\n" help_strs))

let print_err = ANSITerminal.(print_string [red])

let print_invalid_index_error_msg () =
  let msg = "\nInvalid coordinates, please choose again.\n" in
  print_err msg

let print_empty_error_msg () =
  let msg = "\nYou didn't type anything!\n" in
  print_err msg

let print_malformed_error_msg () =
  let msg = "\nInvalid command, please try again.\n" in
  print_err msg

let print_reset_disabled_msg () =
  let msg = "\nResetting is disabled on networked mode!\n" in
  print_err msg

let print_connection_failed_msg () =
  let msg = "\nUnable to connect to that IP address.\n" in
  print_err msg

let print_win_msg p tiebreak =
  let s =
    match p with
    | X -> if tiebreak then "X" else "O"
    | O -> if tiebreak then "O" else "X"
    | Blank -> failwith "Failure: curr_player returned Blank" in
  let msg = "\n" ^ s ^ " is the WINNER!\n" in
  ANSITerminal.(print_string [cyan; Bold] msg)

let print_highscore file t state =
  let high_s = get_high_score file state t in
  let msg = "Your current high score is : " ^ (string_of_float high_s) ^ "\n" in
  ANSITerminal.(print_string [cyan; Bold] msg)

let print_timeout_msg () =
  let msg = "\nOpponent Timeout! You Win!\n" ^ "\nThanks for playing!\n" in
  ANSITerminal.(print_string [cyan] msg)
