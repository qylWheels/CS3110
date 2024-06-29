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
