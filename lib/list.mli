(* List *)

type 'a t

val to_list : 'a t -> 'a list
val empty : 'a t
val is_empty : 'a t -> bool

val hd : 'a t -> 'a option
val tl : 'a t -> 'a t

val cons : 'a -> 'a t -> 'a t

val update : int -> 'a -> 'a t -> 'a t

(* exercise 2.2 *)
val suffixes : 'a t -> 'a t t

(* for testing suffixes 2.2 *)
val map : ('a -> 'b) -> 'a t -> 'b t
