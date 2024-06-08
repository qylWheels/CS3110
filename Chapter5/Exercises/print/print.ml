module type ToString = sig
  type t
  val to_string: t -> string
end

module Print (M : ToString) = struct
  let print v = v |> M.to_string |> print_endline
end
