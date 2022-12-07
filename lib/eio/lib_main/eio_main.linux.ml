let src = Logs.Src.create "eio_main" ~doc:"Effect-based IO main module"
module Log = (val Logs.src_log src : Logs.LOG)

let has_working_uring v =
  (* Note: if you change this, remember to change the log message below too *)
  match String.split_on_char '.' v with
  | "5" :: minor :: _ -> int_of_string minor >= 11
  | major :: _ -> int_of_string major > 5
  | [] -> false

let run_io_uring ?fallback fn =
  Log.info (fun f -> f "Selecting io-uring backend");
  Eio_linux.run ?fallback (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "luv" ->
    failwith "luv has been disabled in this tree"
  | Some _ | None -> run_io_uring fn
