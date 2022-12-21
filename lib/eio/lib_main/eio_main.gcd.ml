let src = Logs.Src.create "eio_main" ~doc:"Effect-based IO main module"
module Log = (val Logs.src_log src : Logs.LOG)

let run_gcd ?fallback fn =
  Log.info (fun f -> f "Selecting gcd backend");
  Eio_gcd.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  run_gcd fn
