let () =
  let headers =
    ["#include <sys/types.h>"; "#include <sys/event.h>"; "#include <sys/time.h>"]
  in
  List.iter print_endline headers ;
  Cstubs_structs.write_c Format.std_formatter (module Kqueue_types.Definition)
