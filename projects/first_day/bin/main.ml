let () = print_endline "Hello, World!";;

let sum a b = a + b;;
let classify_number n =
  if n > 0 then "Positive"
  else if n < 0 then "Negative"
  else "Zero";;
let () =
  let result = classify_number (sum 1 2) in
  print_endline result;;

