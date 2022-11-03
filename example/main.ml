open Eio

let spawn ~clock min max =
  Switch.run @@ fun sw ->
  for i = min to max do
    Fiber.fork ~sw (fun () -> Time.sleep clock (float_of_int i));
    Time.sleep clock (float_of_int (max - i))
  done

(* Based on the Tokio Console example application *)
let main clock =
  Fiber.both
    (fun () -> spawn ~clock 1 10)
    (fun () -> spawn ~clock 10 30)

let () =
  Eio_main.run @@ fun env ->
  Ctf.with_tracing @@ fun () ->
  let clock = Stdenv.clock env in
  main clock