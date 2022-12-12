type game_t = (char * char)
[@@deriving show]

module Part_one = struct
  let draw_score = 3
  let win_score = 6

  let score (game : game_t) =
    (* Printf.printf "Game: %s\n" (show_game_t game); *)
    match game with
    (** Draw *)
    | ('A', 'X') -> draw_score + 1
    | ('B', 'Y') -> draw_score + 2
    | ('C', 'Z') -> draw_score + 3

    (** We win *)
    | ('A', 'Y') -> win_score + 2
    | ('B', 'Z') -> win_score + 3
    | ('C', 'X') -> win_score + 1

    (** We loose *)
    | ('A', 'Z') -> 3
    | ('B', 'X') -> 1
    | ('C', 'Y') -> 2

    | _ -> 0

  let rec exec(guide : game_t list) : int = 
    let individual_scores = List.map score guide in
    let total_score = List.fold_left (+) 0 individual_scores in
    total_score
end

module Part_two = struct
  include Part_one

  exception UnrecognizedMove of char

  let generate_move (game : game_t) =
    (* Printf.printf "Game: %s\n" (show_game_t game); *)
    let their_move, my_strategy = game in 
    match their_move with
    | 'A' -> 
      (match my_strategy with 
      | 'X' -> their_move, 'Z'
      | 'Y' -> their_move, 'X'
      | 'Z' -> their_move, 'Y'
      | _ -> raise (UnrecognizedMove my_strategy))

    | 'B' -> 
      (match my_strategy with 
      | 'X' -> their_move, 'X'
      | 'Y' -> their_move, 'Y'
      | 'Z' -> their_move, 'Z'
      | _ -> raise (UnrecognizedMove my_strategy))
    | 'C' -> 
      (match my_strategy with 
      | 'X' -> their_move, 'Y'
      | 'Y' -> their_move, 'Z'
      | 'Z' -> their_move, 'X'
      | _ -> raise (UnrecognizedMove my_strategy))
    | _ -> raise (UnrecognizedMove their_move)

  let rec exec(guide : game_t list) : int = 
    let expected_games = List.map generate_move guide in
    let individual_scores = List.map score expected_games in
    let total_score = List.fold_left (+) 0 individual_scores in
    total_score
end