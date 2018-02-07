type 'a t = 'a list

let to_list t = t

let empty = []

let is_empty = function [] -> true | _ -> false

let hd = function [] -> None | x :: _ -> Some x

let tl = function [] -> [] | _ :: tl -> tl

let cons a t = a :: t

exception IndexError

(* update, suffixes, & map are not tail recursive. *)

let rec update i a t =
  match (i, t) with
  | _, [] -> raise IndexError
  | 0, _ :: tl -> a :: tl
  | i, x :: tl -> x :: update (i - 1) a tl


let rec suffixes = function [] -> [[]] | _ :: tl as t -> t :: suffixes tl

let rec map f = function [] -> [] | x :: tl -> f x :: map f tl
