(* Inspired by https://github.com/ocaml-multicore/ocaml-uring/blob/main/tests/urcat.ml thanks @avsm *)

let cat ~group path =
  let queue = Dispatch.Queue.create () in
  let io = Dispatch.Io.create_with_path ~flags:0o666 ~mode:0 ~path Stream queue in
  let stdout = Dispatch.Io.(create Stream Fd.stdout queue) in
  Dispatch.Group.enter group;
  Dispatch.Io.set_high_water io (1024 * 64);
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
  let group = Dispatch.Group.create () in
  let path = 
    if Array.length Sys.argv > 1
    then Sys.argv.(1)
    else failwith "Please provide a file to read"
  in
  cat ~group path;
  Dispatch.(Group.wait group (Time.forever ()) |> ignore)
