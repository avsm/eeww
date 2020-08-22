let () =
  Cstubs.write_ml ~errno:Cstubs.return_errno ~concurrency:Cstubs.unlocked
    Format.std_formatter ~prefix:Sys.argv.(1)
    (module Kqueue_stubs.Definition)
