open Types
(*
  syntax sugar for the  implemented version
  let player_to_char = function
    | X -> "X"
    | O -> "O"
 *)
let player_to_char x = 
  match x with
  | X -> "X"
  | O -> "O"

let cell_to_string cell = 
   match cell with 
  | Empty -> " "
  | Taken p -> player_to_char p

let print_cell cell =
  print_string(cell_to_string cell)

let rec print_row row = 
  match row with 
  | [] ->  print_newline ()(* end of row*)
  | [cell] -> print_cell cell 
            (* Rather than just print_string x which is valid,  
                 you can return the () type to make it clear that this function was only run for side effects
                 and returns no useful value ie.
              | [x] -> 
                  let () = print_string (cell_to_string x) in 
                  ()
            *)
  | h :: t -> 
      let () = print_cell  h in 
      let () = print_string "  |  " in
      print_row t 
      (* 
         At first glance, this might seem confusing, but here's a breakdown:
         
         The `let () = ... in` construct is used to handle side effects explicitly in OCaml. 
         When you write `let () = ... in`, you are:
         
         1. **Performing Side Effects**: `print_string` and other functions that produce `unit`
         are executed for their side effects (like printing to the console). The result of these 
         operations is `unit`, which means they don’t produce a meaningful value.
         
         2. **Binding to `unit`**: The `let () = ...` construct binds the result of the side effect 
         (which is `unit`) to `()`. This indicates that you are acknowledging the side effect but not 
         using the result of the operation.
         
         3. **Function Return Value**: The entire function ends up returning `unit` because each 
         branch of the `match` expression (including recursive calls) either directly returns 
         `()` or results in a continuation that is also of type `unit`.
      *)

(* print the new line *) 
let print_separator () = 
  print_string " \n----=----=----\n"

let rec print_board board = 
  match board with
  | [] ->  () (* end of board *)
  | [row] ->  
      print_row row;
      print_separator ()
  | row :: rest ->
      print_row row;
      print_separator () ; 
      print_board rest
