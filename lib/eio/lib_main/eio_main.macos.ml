let src = Logs.Src.create "eio_main" ~doc:"Effect-based IO main module"
module Log = (val Logs.src_log src : Logs.LOG)

let run_gcd fn =
  Eio_gcd.run (fun env -> fn (env :> Eio.Stdenv.t))

let run_luv fn =
  Eio_luv.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "gcd" -> 
    Logs.info (fun f -> f "Using GCD backend");
    run_gcd fn
  | Some "luv" ->
    Log.info (fun f -> f "Using luv backend");
    run_luv fn
  | None | Some "" ->
      Log.info (fun f -> f "Selecting GCD backend");
      run_gcd fn
  | Some x -> Fmt.failwith "Unknown eio backend %S (from $EIO_BACKEND)" x
