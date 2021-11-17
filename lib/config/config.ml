module C = Configurator.V1

let kqueue = {|
#include <sys/event.h>

int main() {
  int k = kqueue();
  return 0;
}
|}

let () =
  let conf = C.create "kqueue.conf" in
  let programs = [ "KQUEUE_AVAILABLE", kqueue ] in
  List.iter
    (fun (var, prog) ->
      if C.c_test conf prog
      then Printf.printf "#define %s\n" var
      else Printf.printf "#undef %s\n" var)
    programs
;;
