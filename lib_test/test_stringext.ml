open OUnit2

let printer = String.concat "-"

let test_split_1 _ =
  let strings = Stringext.split "test:one:two" ~on:':' in
  assert_equal ~printer strings ["test";"one";"two"]

let test_split_bounded_1 _ =
  let strings = Stringext.split "testing:foo:bar" ~on:':' ~max:2 in
  assert_equal ~printer strings ["testing";"foo:bar"]

let test_split_none _ =
  let s = "foo:bar" in
  assert_equal ~printer [s] (Stringext.split s ~on:'=')

let split_trim_left _ =
  let strings = Stringext.split_trim_left
                  "testing, stuff;  \t again" ~on:",;" ~trim:" \t" in
  assert_equal ~printer strings ["testing";"stuff";"again"]

let test_fixtures =
  "test various string functions" >:::
  [
    "test split char 1" >:: test_split_1;
    "test split bounded 1" >:: test_split_bounded_1;
    "test split none" >:: test_split_none;
    "split trim left" >:: split_trim_left;
  ]

let _ = run_test_tt_main test_fixtures


