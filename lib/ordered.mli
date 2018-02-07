type ordering = Less | Equal | Greater

module type S = sig
  type t
  val compare: t -> t -> ordering
end