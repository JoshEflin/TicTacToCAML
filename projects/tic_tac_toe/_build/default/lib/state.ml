open Types
type game_status = Over | Ongoing 
type game_state =  {
  board : board;
  current_player: player;
  game_status : game_status;
}

let empty_board: board = 
  [
    [Empty ; Empty; Empty];
    [Empty; Empty; Empty];
    [Empty; Empty; Empty]
  ]

let initial_state () = {
  board = empty_board;
  current_player = X;
  game_status = Ongoing 
}

let switch_player player = 
  match player with
  | X -> O
  | O -> X



let update_board board row col player =
  let rec update_row r_idx = function
    | [] -> []  (* Base case: empty list *)
    | row :: tail when r_idx = 0 ->
      (* Update the specific column in this row *)
      let rec update_cell c_idx = function
        | [] -> []  (* Base case: empty list *)
        | _ :: tail when c_idx = 0 -> Taken player :: tail  (* Update the cell *)
        | head :: tail -> head :: update_cell (c_idx - 1) tail
      in
      update_cell col row :: tail
    | row :: tail -> row :: update_row (r_idx - 1) tail
  in
  update_row row board
  
let all_equal  = function 
  |[ Taken p1; Taken p2; Taken p3] when p1 = p2 && p2 = p3 -> Some p1
  | _ -> None

let check_winner row = 
  all_equal row

let check_column board col_idx =
  let rec get_column rows acc =
    match rows with
    | [] -> List.rev acc  (* Base case: empty board *)
    | row :: rest ->
        let rec get_cell row idx =
          match row with
          | [] -> failwith "out of bounds"  (* Safeguard: should not happen if used correctly *)
          | h :: t -> if idx = 0 then h else get_cell t (idx - 1)
        in
        (* Extract the cell from the current row and continue with the rest of 
           the rows
           If there is a failure, we return Empty instead
        *)
        let cell = try get_cell row col_idx with Failure _ -> Empty in
        get_column rest ( cell :: acc )
  in

  (* Retrieve the list of cells in the specified column *)
  let column_cells = get_column board [] in
    all_equal column_cells

(* Easy way to check column using the List module.
   Think of this in terms of function application
   for every row in the board, give me the nth thing in the row if n is col_idx
 *)

(*let check_column board col_idx =
  match List.map ( fun row -> List.nth row col_idx) board with
  |[ Taken p1; Taken p2; Taken p3] when p1 = p2 && p2 = p3 -> Some p1
  | _-> None
*)
let get_main_diagonal board =
  let rec aux index rows =
    match rows with
    | [] -> []  (* Base case: no more rows *)
    | row :: rest ->
      match row with
      | [] -> []  (* Safeguard: empty row *)
      | _ ->
        if index >= List.length row then []
        else
          let rec get_element row idx =
            match row with
            | [] -> failwith "Index out of bounds"  (* Safeguard *)
            | hd :: tl -> if idx = 0 then hd else get_element tl (idx - 1)
          in
          let element = get_element row index in
          element :: aux (index + 1) rest
  in
  aux 0 board
let get_anti_diagonal board =
  let size = List.length board in
  let rec aux index rows =
    match rows with
    | [] -> []  (* Base case: no more rows *)
    | row :: rest ->
      match row with
      | [] -> []  (* Safeguard: empty row *)
      | _ ->
        if index >= size then []
        else
          let rec get_element row idx =
            match row with
            | [] -> failwith "Index out of bounds"  (* Safeguard *)
            | hd :: tl -> if idx = 0 then hd else get_element tl (idx - 1)
          in
          let element = get_element row (size - 1 - index) in
          element :: aux (index + 1) rest
  in
  aux 0 board

let check_board board =  
  let rec check_all_rows = function 
    | [] -> None
    | row :: rest ->
        match check_winner row with 
        | Some player -> Some player
        | None -> check_all_rows rest
  in

  let rec check_all_columns col_idx = 
    if col_idx > 2 then
      None (* No winning condition was found *)
    else
      match check_column board col_idx with
      | Some player -> Some player (* Winner found *)
      | None -> check_all_columns (col_idx + 1)
  in

  let check_main_diagonal = 
    match check_winner (get_main_diagonal board) with
    | Some player -> Some player
    | None -> None
  in

  let check_anti_diagonal = 
    match check_winner (get_anti_diagonal board) with
    | Some player -> Some player
    | None -> None
  in

  match check_all_rows board with
  | Some _ as winner -> winner
  | None -> (
      match check_all_columns 0 with
      | Some _ as winner -> winner
      | None -> (
          match check_main_diagonal with
          | Some _ as winner -> winner
          | None -> check_anti_diagonal
        )
    )

