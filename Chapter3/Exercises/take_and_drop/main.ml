let rec take n lst =
  if n = 0 then []
  else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n = 0 then lst
  else
    match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t

let rec string_of_list = function
| [] -> ""
| h :: t -> string_of_int h ^ string_of_list t

let ( -- ) a b = List.init (b - a + 1) (fun i -> i + a)

let rec take_aux n lst acc =
  if n = 0 then List.rev acc
  else
    match lst with
    | [] -> List.rev acc
    | h :: t -> take_aux (n - 1) t (h :: acc)

let take_tr n lst = take_aux n lst []

let drop_tr n lst = drop n lst
