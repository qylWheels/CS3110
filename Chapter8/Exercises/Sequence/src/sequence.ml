type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec nats_gen n = Cons(n, fun () -> nats_gen (n + 1))
let nats = nats_gen 0

let rec filter p s =
  let Cons(v, f) = s in
  if p v = true then Cons(v, fun () -> filter p (f ()))
  else filter p (f ())

let sift n s = filter (fun x -> x mod n <> 0) s

let rec nth n s =
  let Cons(v, f) = s in
  if n = 0 then v
  else nth (n - 1) (f ())

let rec next_prime (Cons(v, f)) =
  Cons(v, fun () -> f () |> sift v |> next_prime)
let nats_from_2 = filter (fun x -> x <> 0 && x <> 1) nats
let primes = next_prime nats_from_2
