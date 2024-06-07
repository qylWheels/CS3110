module type FractionSig = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t
  exception ZeroDenominator

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction : FractionSig = struct
  type t = int * int
  exception ZeroDenominator
  let make n d =
    if d = 0 then raise ZeroDenominator
    else (n, d)
  let numerator = function (n, _) -> n
  let denominator = function (_, d) -> d
  let to_string = function (n, d) -> Printf.sprintf "%i/%i" n d
  let to_float = function (n, d) -> (float_of_int n) /. (float_of_int d)
  let add t1 t2 =
    match (t1, t2) with ((n1, d1), (n2, d2)) -> make (n1 * d2 + n2 * d1) (d1 * d2)
  let mul t1 t2 = match (t1, t2) with ((n1, d1), (n2, d2)) -> make (n1 * n2) (d1 * d2)
end
