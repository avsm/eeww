
let allocdiff =
  let stat1 = Gc.quick_stat () in
  let stat2 = Gc.quick_stat () in
  (stat2.Gc.minor_words -. stat1.Gc.minor_words)

let () =
  Test_bigstring.test1 ();
  let stat1 = Gc.quick_stat () in
  Test_bigstring.test2 ();
  let stat2 = Gc.quick_stat () in
  Printf.printf "bigstring: allocated words %f\n%!" (stat2.Gc.minor_words -. stat1.Gc.minor_words -. allocdiff);
  Test_string.test1 ();
  let stat1 = Gc.quick_stat () in
  Test_string.test2 ();
  let stat2 = Gc.quick_stat () in
  Printf.printf "string: allocated words %f\n%!" (stat2.Gc.minor_words -. stat1.Gc.minor_words -. allocdiff)
