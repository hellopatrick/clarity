module IntHeap = Clarity.Heap.Make (struct
    type t = int

    let compare a b =
      if a < b then Clarity.Ordered.Less
      else if a = b then Clarity.Ordered.Equal
      else Clarity.Ordered.Greater
  end)

let heap = IntHeap.(empty |> insert 4 |> insert 1 |> insert 0 |> insert 10)

let min () =
  Alcotest.(check (option int))
    "find min" (Some 0) (IntHeap.min heap)

let remove_min () =
  let heap = heap |> IntHeap.remove_min |> IntHeap.remove_min |> IntHeap.remove_min in
  Alcotest.(check (option int))
    "find min" (Some 10) (IntHeap.min heap)

let remove_all () =
  let heap = heap
             |> IntHeap.remove_min
             |> IntHeap.remove_min
             |> IntHeap.remove_min
             |> IntHeap.remove_min in
  Alcotest.(check (option int))
    "find min" (None) (IntHeap.min heap)

let of_list () =
  let heap = IntHeap.of_list [1;2;10;-1;2;5;3;0;1;-2;-3;5] in
  Alcotest.(check (option int))
    "find min" (Some (-3)) (IntHeap.min heap)

let to_list () =
  let heap = IntHeap.of_list [1;2;10;-1;2;5;3;0;1;-2;-3;5] in
  let sorted_list = [10;5;5;3;2;2;1;1;0;-1;-2;-3] in
  Alcotest.(check (list int))
    "find min" sorted_list (IntHeap.to_list heap)

let test_set =
  [("Find min in heap.", `Quick, min);
   ("Remove min from heap.", `Quick, remove_min);
   ("Remove all from heap.", `Quick, remove_all);
   ("Build heap from list.", `Quick, of_list);
   ("Convert heap to list", `Quick, to_list);]
