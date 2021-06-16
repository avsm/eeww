(* Inspired by https://github.com/ocaml-multicore/ocaml-uring/blob/main/tests/urcat.ml thanks @avsm *)

let cat ~group fd =
  let queue = Dispatch.Queue.create () in
  let io = Dispatch.Io.create Stream fd queue in
  let stdout = Dispatch.Io.create Stream Unix.stdout queue in
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
  let fd = 
    if Array.length Sys.argv > 1
    then Unix.(openfile Sys.argv.(1) [ O_RDONLY ]) 0
    else Unix.stdin 
  in
  cat ~group fd;
  Dispatch.(Group.wait group (Time.forever ()) |> ignore)
