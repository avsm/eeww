let () =
  let block1 = Dispatch.Block.create (fun () -> print_endline "Hello") in
  let block2 = Dispatch.Block.create (fun () -> print_endline "World") in
  Dispatch.Block.exec block1;
  Dispatch.Block.exec block2
