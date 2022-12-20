(* GC Tests *)
let queue () =
  let q = Dispatch.Queue.create () in
  Dispatch.async q (fun () -> print_endline "Hello");
  Dispatch.sync q (fun () -> print_endline "World")

let random () =
  let group = Dispatch.Group.create () in
  Dispatch.Group.enter group;
  Dispatch.Group.leave group;
  Dispatch.Group.wait group (Dispatch.Time.forever ()) |> ignore;
  let block1 = Dispatch.Block.create (fun () -> print_endline "Hello") in
  let block2 = Dispatch.Block.create (fun () -> print_endline "World") in
  Dispatch.Block.exec block1;
  Dispatch.Block.exec block2

let main () =
  queue ();
  random ()

let () = main ()
