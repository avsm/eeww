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

let printer s = "'" ^ (String.concat ";" s) ^ "'"

let full_split1 _ =
  let strings = Stringext.full_split
                  "//var/test//ocaml/" ~on:'/' in
  assert_equal ~printer
    strings ["/";"/";"var";"/";"test";"/";"/";"ocaml";"/"]

let full_split2 _ =
  let strings = Stringext.full_split "//foobar.com/quux" ~on:'/' in
  assert_equal ~printer
    strings ["/";"/";"foobar.com";"/";"quux"]

let full_split3 _ =
  let strings = Stringext.full_split "foobar.com/quux" ~on:'/' in
  assert_equal ~printer
    strings ["foobar.com";"/";"quux"]

let full_split4 _ =
  let strings = Stringext.full_split "a/path/fragment" ~on:'/' in
  assert_equal ~printer
    strings ["a";"/";"path";"/";"fragment"]

let (to_list, of_list) = Stringext.(to_list, of_list)

let to_list1 _ = assert_equal (to_list "ocaml") ['o';'c';'a';'m';'l']

let to_list2 _ = assert_equal (to_list "") []

let of_list1 _ = assert_equal (of_list []) ""

let of_list2 _ = assert_equal (of_list ['o';'c';'a';'m';'l']) "ocaml"

let test_fixtures =
  "test various string functions" >:::
  [
    "test split char 1" >:: test_split_1;
    "test split bounded 1" >:: test_split_bounded_1;
    "test split none" >:: test_split_none;
    "split trim left" >:: split_trim_left;
    "full split1" >:: full_split1;
    "full split2" >:: full_split2;
    "full split3" >:: full_split3;
    "full split4" >:: full_split4;
    "to_list1" >:: to_list1;
    "to_list2" >:: to_list2;
    "of_list1" >:: of_list1;
    "of_list2" >:: of_list2;
  ]

let _ = run_test_tt_main test_fixtures


