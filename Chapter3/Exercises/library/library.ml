let fifth l =
  if List.length l >= 5 then List.nth l 4
  else 0

let desc_sort l = List.sort Stdlib.compare l |> List.rev
