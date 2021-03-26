let copy_file block_size _queue_depth in_path out_path () =
  (* The main, serial queue for doing the callbacks *)
  let serial = Dispatch.Queue.(create ~typ:Serial ()) in
  (* Io value: TODO implement dispatch_io_create_with_path probably? *)
  let in_fd = Unix.(openfile in_path [ O_RDONLY ] 0) in
  let read_io = Dispatch.Io.(create Stream in_fd serial) in
  let group = Dispatch.Group.create () in
  let write_group = Dispatch.Group.create () in
  let out_fd = Unix.(openfile out_path [ O_RDWR; O_CREAT; O_TRUNC ] 0o775) in
  let write_io = Dispatch.Io.(create Stream out_fd serial) in
  (* Set high-water mark for low memory consumption... I think... *)
  Dispatch.Io.set_high_water read_io block_size;
  Dispatch.Io.with_read serial read_io group
    ~f:(fun data ->
      if Dispatch.Data.size data > 0 then
        Dispatch.Data.apply
          (fun data -> Dispatch.Io.write serial write_io 0 write_group data)
          data)
    ~err:(fun () -> print_endline "Oh no!");
  Dispatch.(Group.wait group @@ Time.forever ()) |> ignore;
  Dispatch.(Group.wait write_group @@ Time.forever ()) |> ignore

let run_cp block_size _queue_depth infile outfile =
  copy_file block_size _queue_depth infile outfile

(* if size < 1_000_000 then run_cp2 _block_size _queue_depth infile outfile
   else run_cp1 _block_size _queue_depth infile outfile *)
