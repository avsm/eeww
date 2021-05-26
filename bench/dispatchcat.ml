(* Inspired by https://github.com/ocaml-multicore/ocaml-uring/blob/main/tests/urcat.ml thanks @avsm *)

let cat ~group fd =
  let queue = Dispatch.Queue.create () in
  let io = Dispatch.Io.create Stream fd queue in
  let stdout = Dispatch.Io.create Stream Unix.stdout queue in
  Dispatch.Group.enter group;
  Dispatch.Io.with_read ~off:0 ~length:max_int ~queue
    ~f:(fun ~err:_ ~finished data ->
      if finished then Dispatch.Group.leave group
      else
        ( Dispatch.Group.enter group;
          Dispatch.Io.with_write ~off:0 ~data ~queue
            ~f:(fun ~err:_ ~finished:_ _ -> Dispatch.Group.leave group) )
          stdout)
    io

let () =
  let fname = Sys.argv.(1) in
  let group = Dispatch.Group.create () in
  let fd = Unix.(openfile fname [ O_RDONLY ]) 0 in
  cat ~group fd;
  Dispatch.(Group.wait group (Time.forever ()) |> ignore)
