let run_gcd fn =
  Eio_gcd.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "gcd" ->
    run_gcd fn
  | None | Some "" ->
      run_gcd fn
  | Some x -> Fmt.failwith "Unknown eio backend %S (from $EIO_BACKEND)" x
