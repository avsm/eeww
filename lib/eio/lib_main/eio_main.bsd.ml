let run_kqueue fn =
  Eio_kqueue.run (fun env -> fn (env :> Eio.Stdenv.t))

let run fn =
  match Sys.getenv_opt "EIO_BACKEND" with
  | Some "kqueue" -> run_kqueue fn
  | None | Some "" -> run_kqueue fn
  | Some x -> Fmt.failwith "Unknown eio backend %S (from $EIO_BACKEND)" x
