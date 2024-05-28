let rec h n pp p =
  if n = 1 then p
  else h (n - 1) p (pp + p)
in
let fib n = h n 0 1 in
assert (fib 1 = 1);
assert (fib 2 = 1);
assert (fib 3 = 2);
assert (fib 4 = 3);
assert (fib 75 = 2111485077978050);
Printf.printf "int max = %i\n" Int.max_int
