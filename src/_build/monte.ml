open State
open Board 

(** The type of a visit counter. *)
type visits = float

(** The type of a win counter. *)
type wins = float

(** The type of a tree of game states. *)
type tree = 
  | Leaf
  | Node of State.t*(int*int)*visits ref*wins ref*tree*tree list ref

let cartesian_product l1 l2 =
  List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) l2) l1)

(** [i -- j] is the list of integers from [i] to [j], inclusive. *)
let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i > j then l
    else from i (j - 1) (j :: l)
  in from i j []

(** [next_possible_moves gm] is a list of possible coordinates the AI can 
    move to given current game [gm]. *)
let next_possible_moves gm = 
  match curr_board gm with
  | i when i >= 0 && i < 9 ->
    let x_low = 3 * (i mod 3) in
    let x_lst = (x_low -- (x_low + 2)) in
    let y_low = 3 * (i / 3) in
    let y_lst = (y_low -- (y_low + 2)) in
    cartesian_product x_lst y_lst
  | _ -> cartesian_product (0 -- 8) (0 -- 8)

(** [next_possible_states n] is a list of the next possible states the game can
    transition to given the current state [n]. *)
let next_possible_states (Node (gm, (x,y), v, w, p, lst)) = 
  let rec states acc = function
    | [] -> acc
    | (x',y') :: xs -> 
      begin
        try
          let s = create_state gm in
          player_move x' y' s false;
          states 
            (Node (s, (x',y'), ref 0., ref 0., 
                   Node (gm, (x,y), v, w, p, lst), ref [])::acc) xs
        with Invalid_Move -> states acc xs
      end
  in states [] (next_possible_moves gm)

(** [random_move gm] is a random coordinate for the AI to move given the current
    state of the game [gm]. *)
let random_move gm =
  match curr_board gm with
  | n when n >= 0 && n < 9 ->
      let fstcoord = (Random.int 3) + 3 * (n mod 3) in
      let sndcoord = (Random.int 3) + 3 * (n / 3) in
      fstcoord, sndcoord
  | _ -> Random.int 9, Random.int 9

(** [get_children n] is the list of children of node [n]. 
    Requires: [n] is not [Leaf]. *)
let get_children (Node (_, _, _, _, _, lst)) = !lst 

(** [select n] is the child of node [n] with the highest upper confidence bound. 
    Requires: [n] is not [Leaf]. *)
let select (Node (_, _, v, _, _, lst)) = 
  fst (List.fold_left 
         (fun (n, ucb) (Node (t, (x,y), v', w', p, l)) -> 
            let 
              ucb' = 
              if !v' = 0. then max_float 
              else (!w' /. !v') +. sqrt (2. *. log(!v) /. !v') 
            in
            if ucb' > ucb then 
              (Node (t, (x,y), v', w', p, l), ucb') else (n, ucb) )
         (Leaf, 0.) !lst)

(** [expand n] sets the children of node [n] to all possible subsequent moves.
    Requires: [n] is not [Leaf]. *)
let expand (Node (gm, (x,y), v, w, p, lst)) = 
  lst := next_possible_states (Node (gm, (x,y), v, w, p, lst)) 

(** [get_game n] is the current game state at node [n]. 
    Requires: [n] is not [Leaf]. *)
let get_game (Node (gm, _, _, _, _, _)) = gm

(** [simulation_node n] is the node that the AI runs the simulation on given 
    root node [n]. *)
let simulation_node node =
  let n = ref node in 
  begin
    try
      while get_children !n <> [] do
        n := let x = select !n in if x = Leaf then failwith "break" else x
      done
    with Failure _ -> ()
  end;
  if (!n |> get_game |> get_small_board |> check_win |> not) &&
     (!n |> get_game |> get_small_board |> is_full |> not) then 
    begin
      expand !n;
      let lst = get_children !n in
      if List.length lst <> 0 then 
        n := List.nth lst (Random.int (List.length lst))
      else ();
    end
  else ();
  !n

(************************Simulation and Backpropagation************************)

(** [simulate n] is the winner after running a random simulated game on node 
    [n].
    Requires: [n] is not [Leaf]. *)
let simulate (Node (gm, _, _, _, _, lst)) = 
  let game = create_state gm in 
  while 
    (game |> get_small_board |> check_win |> not) &&
    (game |> get_small_board |> is_full |> not) 
  do
    try
      let (x,y) = random_move (game) in player_move x y game false;
    with Invalid_Move -> ()
  done;
  if game |> get_small_board |> check_win then 
    begin
      if curr_player game = X then O 
      else X
    end
  else
    tie_breaker (get_small_board game)

(** [update_win w] updates the winning score. *)
let update_win w = w := !w +. 1.

(** [update_visit v] updates the number of visits. *)
let update_visit v = v := !v +. 1.

(** [backpropagate n sq] updates the winning score and number of visits for 
    each node given node [n] and winning player [sq]. 
    Requires: [n] is not [Leaf]. *)
let rec backpropagate (Node (gm, _, v, w, p, _)) sq = 
  match p with
  | Leaf ->
    begin
      update_visit v;
      if curr_player gm <> sq then update_win w
    end
  | node  ->
    begin
      update_visit v;
      if curr_player gm <> sq then update_win w; 
      backpropagate node sq
    end

(** [next_coords n] is the coordinate of the move to reach the state in node [n]
    from the state in the parent node of [n]. 
    Requires: [n] is not [Leaf]. *)
let next_coords (Node (_, _, _, _, _, lst)) = 
  fst (List.fold_left 
         (fun (coord, max) (Node (_, xy, v, _, _, _)) -> 
            if max > !v then 
              (coord, max) else (xy, !v) )
         ((0,0), 0.) !lst)

let next_move state =
  ignore (Random.self_init());
  let timer = ref 100 in
  let root = Node (create_state state, (0,0), ref 0., ref 0., Leaf, ref []) 
  in
  while !timer <> 0 do
    let node = simulation_node root in
    backpropagate node (simulate node);
    timer := !timer - 1 
  done;
  next_coords root
