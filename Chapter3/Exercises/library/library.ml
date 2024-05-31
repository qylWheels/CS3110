let fifth l =
  if List.length l >= 5 then List.nth l 4
  else 0

let desc_sort l = List.sort Stdlib.compare l |> List.rev

let last l = List.nth l ((List.length l) - 1)

let rec any_zeroes = function
  | [] -> false
  | h :: t -> h = 0 || any_zeroes t
