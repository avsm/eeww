let () =
  let headers =
    ["#include <sys/types.h>"; "#include <sys/event.h>"; "#include <sys/time.h>"]
  in
  List.iter print_endline headers ;
  Cstubs.write_c ~concurrency:Cstubs.unlocked Format.std_formatter
    ~prefix:Sys.argv.(1)
    (module Kqueue_stubs.Definition)
