#include <sys/random.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

CAMLprim value caml_eio_arc4random(value v_ba, value v_off, value v_len) {
  CAMLparam1(v_ba);
  ssize_t off = (ssize_t)Long_val(v_off);
  ssize_t len = (ssize_t)Long_val(v_len);
  void *buf = Caml_ba_data_val(v_ba) + off;
  caml_enter_blocking_section();
  // TODO: no return value from arc4random!
  arc4random_buf(buf, len);
  caml_leave_blocking_section();
  CAMLreturn(v_len);
}