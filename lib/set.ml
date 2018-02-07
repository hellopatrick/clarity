module type S =
sig
  type elt
  type t

  val empty : t
  val member : elt -> t -> bool
  val insert : elt -> t -> t
end

module Make (Elt : Ordered.S) : S with type elt = Elt.t =
struct
  type elt = Elt.t
  type t = Empty | Tree of t * elt * t

  let empty = Empty

  let rec member elt = function
    | Empty -> false
    | Tree (l, x, r) ->
      match Elt.compare elt x with
      | Ordered.Equal -> true
      | Ordered.Less -> member elt l
      | Ordered.Greater -> member elt r


  exception ElementExists

  (* Exercise 2.3: raise exn if seen the item and return original set. *)

  let rec insert elt t =
    let rec aux t =
      match t with
      | Empty -> Tree (Empty, elt, Empty)
      | Tree (l, x, r) ->
        match Elt.compare elt x with
        | Ordered.Equal -> raise ElementExists
        | Ordered.Less -> Tree (aux l, x, r)
        | Ordered.Greater -> Tree (l, x, aux r)
    in
    try aux t with ElementExists -> t
end
