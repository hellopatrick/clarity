module type S =
sig
  type key
  type 'a t

  val empty: 'a t
  val bind: key -> 'a -> 'a t -> 'a t
  val lookup: key -> 'a t -> 'a option
end

module Make (Key : Ordered.S) : S with type key = Key.t