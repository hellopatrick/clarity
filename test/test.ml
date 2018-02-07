let () =
  Alcotest.run "Clarity" [("lists", Lists.test_set); ("sets", Sets.test_set)]
