let lazy_hello = lazy (print_endline "Hello lazy world!")

let ( &&& ) a b =
  let a_result = Lazy.force a in
  if a_result = false then false
  else Lazy.force b
