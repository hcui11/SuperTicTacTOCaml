open State
open Board

let random_move i = 
  match i with
  | n when n >= 0 && n < 9 ->
      let fstcoord = (Random.int 3) + 3 * (n mod 3) in
      let sndcoord = (Random.int 3) + 3 * (n / 3) in
      fstcoord, sndcoord
  | _ -> Random.int 9, Random.int 9

let rec next_move (state : State.t) =
  ignore (Random.self_init());
  let state_copy = create_state state in
  let (x,y) = random_move (curr_board state_copy) in
  try 
    player_move x y state_copy false;
    x,y
  with
    Invalid_Move -> next_move state;
