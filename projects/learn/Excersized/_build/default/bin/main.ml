(* Construct a list that has integers 1 through 4 in it. Square Bracket Notation *)

let my_list = [1;2;3;4;5]
let my_other_list = 1 :: 2 :: 3 :: 4 :: 5 :: []
let my_super_other_list = [1] @ [2;3;4] @ [5]

let rec product = function
  | [] ->  1
  | [x] -> x
  | h :: t -> h * product t
  
