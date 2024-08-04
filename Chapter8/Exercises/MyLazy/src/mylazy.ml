let lazy_hello = lazy (print_endline "Hello lazy world!")

let ( &&& ) a b =
  let a_result = Lazy.force a in
  if a_result = false then false
  else Lazy.force b

type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

let rec map f seq =
  let Cons (h, t) = seq in
  Cons (f h, lazy (map f (Lazy.force t)))

let rec filter p seq =
  let Cons (h, t) = seq in
  if p h then Cons (h, lazy (filter p (Lazy.force t)))
  else filter p (Lazy.force t)
