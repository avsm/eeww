let () =
  let group = Dispatch.Group.create () in
  Dispatch.Group.enter group;
  Dispatch.Group.leave group;
  Dispatch.Group.wait group (Dispatch.Time.forever ()) |> ignore;
  let block1 = Dispatch.Block.create (fun () -> print_endline "Hello") in
  let block2 = Dispatch.Block.create (fun () -> print_endline "World") in
  Dispatch.Block.exec block1;
  Dispatch.Block.exec block2;
  let d1 = Dispatch.Data.create (Cstruct.to_bigarray (Cstruct.of_string "hello1")) in 
  let d2 = Dispatch.Data.create (Cstruct.to_bigarray (Cstruct.of_string "hello2")) in 
  let d3 = Dispatch.Data.concat d1 d2 in 
  Dispatch.Data.apply (fun d -> print_int (Dispatch.Data.size d)) d3
