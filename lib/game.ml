open Types
open State
(*This file is for Game logic*)

let prompt_user () =
  print_string "Enter the Row and Column as space separated integers";
  let input = read_line () in
  match String.split_on_char ' ' input with
  | [row; col] -> (int_of_string row, int_of_string col)
  | _ -> failwith "Make sure your coordinates are formated as follows '1 2'"

let make_move state row col =
  if state.game_status = Ongoing && List.nth (List.nth state.board row) col = Empty then
    let new_board = update_board state.board row col state.current_player in
    let new_player = switch_player state.current_player in
    let new_game_status = match check_board new_board with
      | Some _ -> Over
      | None -> Ongoing
    in
    {board = new_board; current_player = new_player; game_status = new_game_status}
  else
    state

let rec play_game state =
  Display.print_board state.board;
  if state.game_status = Over then
    print_endline "Game Over!"
  else
    let row, col = prompt_user () in
    let new_state = make_move state row col in
    play_game new_state

