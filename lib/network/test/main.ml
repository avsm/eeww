open Network

let () =
  let listener = Listener.create_with_port ~port:9000 (Parameters.create ()) in
  Listener.start listener;
  print_int (Listener.get_port listener)
