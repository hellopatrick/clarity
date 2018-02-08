type 'elt t = Empty | Node of 'elt t * 'elt * 'elt t

(* Exercise 2.5a : Build a complete tree, by sharing *)

let rec complete elt depth =
  match depth with
  | 0 -> Node (Empty, elt, Empty)
  | d ->
      let sub = complete elt (d - 1) in
      Node (sub, elt, sub)


let inorder_traversal t =
  let rec aux t res =
    match t with
    | Empty -> res
    | Node (l, x, r) ->
        let res' = x :: aux l res in
        aux r res'
  in
  aux t []
