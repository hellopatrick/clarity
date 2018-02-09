module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool

  val insert : elt -> t -> t
  val merge : t -> t -> t

  val min : t -> elt option
  val remove_min : t -> t
end

module Make (Elt : Ordered.S) : S with type elt = Elt.t = struct
  type elt = Elt.t
  type t = Empty | Node of (int * Elt.t * t * t)

  let empty = Empty
  let is_empty t = t = Empty

  let rank = function Empty -> 0 | Node (r, _, _, _) -> r

  let leftist x a b =
    let ra = rank a and rb = rank b in
    if ra >= rb then Node (rb + 1, x, a, b) else Node (ra + 1, x, b, a)

  let rec merge a b =
    match (a, b) with
    | Empty, b -> b
    | a, Empty -> a
    | Node (_, xa, la, ra), Node (_, xb, lb, rb) ->
      match Elt.compare xa xb with
      | Ordered.Less | Ordered.Equal -> leftist xa la (merge ra b)
      | Ordered.Greater -> leftist xb lb (merge a rb)

  let insert x t = Node (1, x, Empty, Empty) |> merge t

  let min = function Empty -> None | Node (_, x, _, _) -> Some x

  let remove_min = function Empty -> Empty | Node (_, _, l, r) -> merge l r
end
