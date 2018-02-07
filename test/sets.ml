module IntSet = Clarity.Set.Make (struct
    type t = int

    let compare a b =
      if a < b then Clarity.Ordered.Less
      else if a = b then Clarity.Ordered.Equal
      else Clarity.Ordered.Greater
  end)

let set = IntSet.(empty |> insert 4 |> insert 3 |> insert 10 |> insert 7)

let member () = Alcotest.(check bool) "is_member" true (IntSet.member 4 set)

let insert_same () =
  Alcotest.(check (testable Fmt.nop ( == )))
    "same_object" set (IntSet.insert 10 set)


let test_set =
  [ ("Can find an element", `Quick, member)
  ; ("Don't copy unless new element", `Quick, insert_same) ]
