module IntMap = Clarity.Map.Make (struct
  type t = int

  let compare a b =
    if a < b then Clarity.Ordered.Less
    else if a = b then Clarity.Ordered.Equal
    else Clarity.Ordered.Greater
end)

let map = IntMap.(empty |> bind 4 3 |> bind 2 4 |> bind 10 2 |> bind 7 9)

let lookup () =
  Alcotest.(check (list (option int)))
    "is_member" [Some 3; Some 4; Some 2; Some 9]
    (ListLabels.map ~f:(fun key -> IntMap.lookup key map) [4; 2; 10; 7])


let bind () =
  let map = IntMap.bind 10 9 map in
  Alcotest.(check (option int)) "is_member" (Some 9) (IntMap.lookup 10 map)


let test_set =
  [("Lookup finds a key", `Quick, lookup); ("Bind updates a key", `Quick, bind)]
