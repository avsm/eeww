open Eio

let () =
  Eio_gcd.run @@ fun env ->
  Flow.copy_string "Hello GCD!" env#stdout