module type ToString = sig
  type t
  val to_string: t -> string
end

module Print (M : ToString) = struct
  let print v = v |> M.to_string |> print_endline
end

module Int = struct
  type t = int
  let to_string = string_of_int
end

module MyString = struct
  type t = string
  let to_string = Fun.id
end

module PrintInt = Print(Int)
module PrintString = Print(MyString)
module StringWithPrint = struct
  include String
  include PrintString
end
