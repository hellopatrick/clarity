type 'elt t = Empty | Node of 'elt t * 'elt * 'elt t

val complete : 'elt -> int -> 'elt t

val inorder_traversal : 'elt t -> 'elt list
