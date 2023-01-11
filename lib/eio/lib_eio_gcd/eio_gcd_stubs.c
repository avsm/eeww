#include <Security/Security.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>

// Must be a uint8 bigarray
CAMLprim value caml_eio_gcd_secure_random(value v_len, value v_ba) {
    CAMLparam1(v_ba);
    int status = SecRandomCopyBytes(kSecRandomDefault, Int_val(v_len), Caml_ba_data_val(v_ba));
    if (status != 0) { // Always test the status.
        caml_failwith("caml_eio_gcd_secure_random: failed to fill secure random buffer!");
    }
    CAMLreturn(Val_unit);
}