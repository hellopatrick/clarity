module type S = sig
  type key

  type 'a t

  val empty : 'a t

  val bind : key -> 'a -> 'a t -> 'a t

  val lookup : key -> 'a t -> 'a option
end

module Make (Key : Ordered.S) : S with type key = Key.t = struct
  type key = Key.t

  type 'a t = (key * 'a) Tree.t

  let empty = Tree.Empty

  let bind key value t =
    let rec aux t =
      match t with
      | Tree.Empty -> Tree.Node (Empty, (key, value), Empty)
      | Tree.Node (l, (k, v), r) ->
        match Key.compare key k with
        | Ordered.Equal -> Tree.Node (l, (key, value), r)
        | Ordered.Less -> Tree.Node (aux l, (k, v), r)
        | Ordered.Greater -> Tree.Node (l, (k, v), aux r)
    in
    aux t


  let rec lookup key t =
    match t with
    | Tree.Empty -> None
    | Tree.Node (l, (k, v), r) ->
      match Key.compare key k with
      | Ordered.Equal -> Some v
      | Ordered.Less -> lookup key l
      | Ordered.Greater -> lookup key r
end
