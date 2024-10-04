(* Main.ml *)
open Tic_tac_toe


let () =
  let initial = State.initial_state () in Game.play_game initial

