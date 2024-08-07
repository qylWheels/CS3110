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

  (* let ( >>| ) m f =
    match m with
    | Some x -> Some (f x)
    | None -> None

  let join m =
    match m with
    | Some (Some x) -> Some x
    | Some (None) -> None
    | None -> None *)

  let ( >>| ) m f =
    m >>= fun x ->
      x |> f |> return

  let join m =
    m >>= Fun.id
end

(* open Maybe *)

module Add = struct
  open Maybe
  let add a b =
    a >>= fun x ->
      b >>= fun y ->
        return (x + y)
end

module type FmapJoinMonad = sig
  type 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* I can't understand this *)
module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  include M
  let return = return
  let ( >>= ) m f =
    m >>| f |> join
end

module type ExtMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return l = [l]

  let rec ( >>= ) l f =
    match l with
    | [] -> []
    | h :: t -> (f h) @ (t >>= f)

  let ( >>| ) l f =
    l >>= fun x ->
      x |> f |> return

  let join l =
    l >>= Fun.id
end
