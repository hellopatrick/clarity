let () =
  Alcotest.run "Clarity"
    [ ("lists", Lists.test_set)
    ; ("sets", Sets.test_set)
    ; ("maps", Maps.test_set)
    ; ("heaps", Heaps.test_set) ]
