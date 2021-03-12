let get_size fd = Unix.(handle_unix_error fstat fd) |> fun t -> t.st_size
let read_size = 512 * 1024

module Io_q = struct
  type req = {
    typ : [ `R of Dispatch.Data.t | `W of Dispatch.Data.t ];
    len : int;
    off : int;
  }

  type t = {
    reqs : req array;
    queue : Dispatch.Queue.t;
    mutable outstanding : int;
  }

  let create size f queue = { reqs = Array.init size f; queue; outstanding = 0 }

  let copy q channel =
    let f req =
      match req.typ with
      | `R data ->
          let group = Dispatch.Group.create () in
          Dispatch.Io.read q.queue channel req.len req.off group data;
          group
      | `W data ->
          let group = Dispatch.Group.create () in
          Dispatch.Io.write q.queue channel req.off group data;
          group
    in
    Array.map (fun req -> f req) q.reqs
end

let run_cp1 block_size _queue_depth infile outfile () =
  let in_fd = Unix.(openfile infile [ O_RDONLY ] 0) in
  let bytes = get_size in_fd in
  let blocks =
    if bytes mod block_size <> 0 then (bytes / block_size) + 1
    else bytes / block_size
  in
  let queue = Dispatch.Queue.create () in
  let read_queue =
    Io_q.create blocks
      (fun i ->
        {
          typ =
            `R
              ( Dispatch.Data.create
              @@ Bigarray.(Array1.create char c_layout block_size) );
          len = blocks;
          off = blocks * i;
        })
      queue
  in
  let submit_read req channel q =
    match req.Io_q.typ with
    | `R data ->
        let group = Dispatch.Group.create () in
        Dispatch.Io.read q channel req.Io_q.len req.off group data;
        group
    | _ -> assert false
  in
  let channel = Dispatch.Io.(create Random in_fd queue) in
  Dispatch.Io.set_high_water channel (block_size * 4);
  let groups =
    Array.map
      (fun t ->
        read_queue.outstanding <- read_queue.outstanding + 1;
        (submit_read t channel queue, t, ref false))
      read_queue.reqs
  in
  let out_fd = Unix.(openfile outfile [ O_RDWR; O_CREAT; O_TRUNC ] 0o775) in
  let channel = Dispatch.Io.(create Random out_fd queue) in
  let write_queue = Queue.create () in
  while read_queue.outstanding > 0 do
    (* Printf.printf "%i\n" read_queue.outstanding; *)
    Array.iteri
      (fun _ (g, t, submitted) ->
        if !submitted then ()
        else
          match (Dispatch.(Group.wait g @@ Time.now ()), t.Io_q.typ) with
          | Ready, `R data ->
              let group = Dispatch.Group.create () in
              Dispatch.Io.write queue channel t.off group data;
              read_queue.outstanding <- read_queue.outstanding - 1;
              submitted := true;
              Queue.add group write_queue
          | Pending, _ -> ()
          | _ -> ())
      groups;
    ()
  done;
  while not @@ Queue.is_empty write_queue do
    Queue.pop write_queue |> fun group ->
    Dispatch.(Group.wait group @@ Time.dispatch_forever ()) |> fun state ->
    assert (state = Ready)
  done

let run_cp2 _block_size _queue_depth infile outfile () =
  let queue = Dispatch.Queue.global () in
  let read_group = Dispatch.Group.create () in
  let in_fd = Unix.(openfile infile [ O_RDONLY ] 0) in
  let channel = Dispatch.Io.(create Random in_fd queue) in
  let data =
    Dispatch.Data.create
    @@ Bigarray.(Array1.create char c_layout (get_size in_fd))
  in
  Dispatch.Io.read queue channel 0 0 read_group data;
  Dispatch.(Group.wait read_group @@ Time.forever ()) |> fun t ->
  assert (t = Dispatch.Group.Ready);
  let write_group = Dispatch.Group.create () in
  let out_fd = Unix.(openfile outfile [ O_RDWR; O_CREAT; O_TRUNC ] 0o775) in
  let out_channel = Dispatch.Io.(create Random out_fd queue) in
  Dispatch.Data.(
    apply (fun t -> Dispatch.Io.write queue out_channel 0 write_group t) data);
  Dispatch.(Group.wait write_group @@ Time.forever ()) |> fun t ->
  assert (t = Dispatch.Group.Ready)

let run_cp _block_size _queue_depth infile outfile =
  let in_fd = Unix.(openfile infile [ O_RDONLY ] 0) in
  let size = get_size in_fd in
  if size < 1_000_000 then run_cp2 _block_size _queue_depth infile outfile
  else run_cp1 _block_size _queue_depth infile outfile
