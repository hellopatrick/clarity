module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool

  val insert : elt -> t -> t
  val merge : t -> t -> t

  val min : t -> elt option
  val remove_min : t -> t

  val of_list : elt list -> t
  val to_list : t -> elt list
end

module Make (Elt : Ordered.S) : S with type elt = Elt.t
