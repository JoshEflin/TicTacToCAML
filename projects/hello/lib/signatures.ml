module type Fact = sig

val fact : int -> int
end

module RecusirveFact : Fact = struct
  let rec fact n = 
    if n = 0 then 1 else 
      n * fact (n -1)
end

 (*module NotFact : Fact = struct
  let incr x = x + 1
   end*)
 module TailRecusiveFact : Fact = struct
   let rec fact_aux n acc =
     if n = 0 then acc else
        fact_aux(n-1)(n * acc)

  let fact n = 
    fact_aux n 1
 end

  let x = TailRecusiveFact.fact 15

