open Types
type game_status = Over | Ongoing 
type game_state =  {
  board : board;
  current_player: player;
  game_status : game_status;
}

val all_equal : cell list -> player option
val check_board : board -> player option
val update_board : board -> int -> int -> player -> board
val initial_state : unit -> game_state
val switch_player : player -> player
val check_winner : cell list -> player option
val check_column : board -> int -> player option
val get_main_diagonal : board -> cell list
val get_anti_diagonal : board -> cell list
  
