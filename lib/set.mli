module type S = sig
  type elt
  type t

  val empty : t

  val member : elt -> t -> bool
  val insert : elt -> t -> t
end

module Make (Elt : Ordered.S) : S with type elt = Elt.t
