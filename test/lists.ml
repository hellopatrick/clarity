let list = Clarity.List.(empty |> cons 4 |> cons 3 |> cons 2 |> cons 1)

let create () =
  Alcotest.(check (list int))
    "creates" [1; 2; 3; 4]
    (Clarity.List.to_list list)


let update () =
  Alcotest.(check (list int))
    "update" [1; 2; 9; 4]
    Clarity.List.(list |> update 2 9 |> to_list)


let suffixes () =
  Alcotest.(check (list (list int)))
    "suffixes" [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
    Clarity.List.(list |> suffixes |> map to_list |> to_list)


let test_set =
  [ ("Create", `Quick, create)
  ; ("Update", `Quick, update)
  ; ("Suffixes", `Quick, suffixes) ]
