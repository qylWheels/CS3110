module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let ( >>= ) m f =
    match m with
    | Some x -> f x
    | None -> None

  let ( >>| ) m f =
    match m with
    | Some x -> Some (f x)
    | None -> None

  let join m =
    match m with
    | Some (Some x) -> Some x
    | Some (None) -> None
    | None -> None
end

open Maybe

let add a b =
  a >>= fun x ->
    b >>= fun y ->
      return (x + y)
