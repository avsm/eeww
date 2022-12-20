let copy_file block_size _queue_depth in_path out_path () =
  let serial = Dispatch.Queue.(create ~typ:Serial ()) in
  let read_io = Dispatch.Io.(create_with_path ~flags:0o666 ~mode:0 ~path:in_path Stream serial) in
  let group = Dispatch.Group.create () in
  Dispatch.Group.enter group;
  let write_group = Dispatch.Group.create () in
  let write_io = Dispatch.Io.(create_with_path ~flags:0o666 ~mode:0 ~path:out_path Stream serial) in
  (* Set high-water mark for low memory consumption... I think... *)
  Dispatch.Io.set_high_water read_io block_size;
  Dispatch.Io.with_read ~off:0 ~length:max_int ~queue:serial
    ~f:(fun ~err:_ ~finished data ->
      if Dispatch.Data.size data > 0 then
        Dispatch.Data.apply
          (fun data -> Dispatch.Io.write serial write_io 0 write_group data)
          data;
      if finished then Dispatch.Group.leave group)
    read_io;
  Dispatch.(Group.wait group @@ Time.forever ()) |> ignore;
  Dispatch.(Group.wait write_group @@ Time.forever ()) |> ignore

let run_cp block_size _queue_depth infile outfile =
  let infile = Filename.concat (Sys.getcwd ()) infile in
  let outfile = Filename.concat (Sys.getcwd ()) outfile in
  copy_file block_size _queue_depth infile outfile

(* if size < 1_000_000 then run_cp2 _block_size _queue_depth infile outfile
   else run_cp1 _block_size _queue_depth infile outfile *)
