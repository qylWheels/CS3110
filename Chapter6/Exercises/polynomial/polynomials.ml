(** [Poly] represents immutable polynomials with integer coefficients. *)
module type Polynomials = sig
  (** [t] is the type of polynomials. *)
  type t

  (** [create coe] is polynomials whose coefficients are set to elements in [coe], 
      from higher term to lower term. *)
  val create : int list -> t

  (** [coefficents poly] is coefficients of the specified polynomials. *)
  val coefficients : t -> int list

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
      $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int
end

module Polynomials : Polynomials = struct
  (* AF: [[cn, ..., c1, c0]] represents polynomials [cn * x ^ n + ... + c1 * x ^ 1 + c0]. *)
  (* RI: [t] mustn't be a empty list. *)
  type t = int list

  let create (coe : int list) = coe

  let coefficients (poly : t) = poly

  let eval x p =
    let len = List.length p in
    let rec eval_aux x p pow =
      match p with
      | [] -> 0
      | h :: t ->
        h * (int_of_float ((float_of_int x) ** (float_of_int pow))) + eval_aux x t (pow - 1)
    in
    eval_aux x p (len - 1)
end
