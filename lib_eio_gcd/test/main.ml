open Eio

let () =
  Eio_gcd.run @@ fun env ->
  let test = Path.(env#fs / "test.txt") in 
  let _write =
    Eio.Path.(with_open_out ~create:(`If_missing 0o644) test) @@ fun f ->
    Flow.copy_string "Copying `Hello GCD!' to file\n" env#stdout;
    Flow.copy_string "Hello GCD!\n" f
  in
  let _read = 
    Eio.Path.(with_open_in test) @@ fun f ->
    let buf = Buffer.create 16 in
    Flow.copy f Flow.(buffer_sink buf);
    Eio.traceln "Read test.txt: %s" (Buffer.contents buf)
  in
    Eio.Path.unlink test