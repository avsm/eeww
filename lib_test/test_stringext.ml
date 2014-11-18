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

let s = "testing one two three"

let opt_int = function
  | None -> "none"
  | Some x -> string_of_int x

let find_from1 _ =
  let r = Stringext.find_from s ~pattern:"ocaml" in
  assert_equal r None

let find_from2 _ =
  let r = Stringext.find_from s ~pattern:"testing" in
  assert_equal r (Some 0)

let find_from3 _ =
  let r = Stringext.find_from s ~pattern:"one" in
  assert_equal r (Some 8)

let find_from4 _ =
  let r = Stringext.find_from s ~pattern:"threee" in
  assert_equal r None

let find_from5 _ =
  let pattern = "three" in
  let r = Stringext.find_from s ~pattern in
  assert_equal ~printer:opt_int
    (Some (String.length s - String.length pattern)) r 

let replace_all1 _ =
  let s = "the quick brown fox brown." in
  let s' = Stringext.replace_all s "brown" "blue" in
  assert_equal ~printer:(fun x -> x) "the quick blue fox blue." s'

let replace_all_assoc1 _ =
  let s = "hello from ocaml" in
  let tbl = [("hello", "goodbye"); ("ocaml", "haskell")] in
  let s' = Stringext.replace_all_assoc s tbl in
  assert_equal ~printer:(fun x -> x) "goodbye from haskell" s'

let replace_all_assoc2 _ =
  let s = "one two three" in
  let t = [("one", "four"); ("two", "five"); ("three", "six"); (" ", "_")] in
  let s' = Stringext.replace_all_assoc s t in
  assert_equal ~printer:(fun x -> x) "four_five_six" s'

let test_fixtures =
  "test various string functions" >:::
  [ "test split char 1"    >:: test_split_1
  ; "test split bounded 1" >:: test_split_bounded_1
  ; "test split none"      >:: test_split_none
  ; "split trim left"      >:: split_trim_left
  ; "full split1"          >:: full_split1
  ; "full split2"          >:: full_split2
  ; "full split3"          >:: full_split3
  ; "full split4"          >:: full_split4
  ; "to_list1"             >:: to_list1
  ; "to_list2"             >:: to_list2
  ; "of_list1"             >:: of_list1
  ; "of_list2"             >:: of_list2
  ; "find_from1"           >:: find_from1
  ; "find_from2"           >:: find_from2
  ; "find_from3"           >:: find_from3
  ; "find_from4"           >:: find_from4
  ; "find_from5"           >:: find_from5
  ; "replace_all1"         >:: replace_all1
  ; "replace_all_assoc1"   >:: replace_all_assoc1
  ; "replace_all_assoc1"   >:: replace_all_assoc2 ]

let _ = run_test_tt_main test_fixtures


