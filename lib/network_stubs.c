#include <dispatch/dispatch.h>
#include <Network/Network.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>
#include <string.h>
#include <pthread.h>

#define DEBUG 1

#define Queue_val(v) (*((dispatch_queue_t *)Data_custom_val(v)))
#define Channel_val(v) (*((dispatch_io_t *)Data_custom_val(v)))
#define Group_val(v) (*((dispatch_group_t *)Data_custom_val(v)))
#define Data_val(v) (*((dispatch_data_t *)Data_custom_val(v)))

#define Params_val(v) (*((nw_parameters_t *)Data_custom_val(v)))
#define Listener_val(v) (*((nw_listener_t *)Data_custom_val(v)))
#define Connection_val(v) (*((nw_connection_t *)Data_custom_val(v)))

// ~~~ Listeners ~~~
static struct custom_operations listener_ops = {
    "network.listen",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_network_listener_create(value v_params) {
  CAMLparam1(v_params);
  nw_parameters_t params = Params_val(v_params);
  nw_listener_t listen = nw_listener_create(params);
  CAMLlocal1(v_listen);
  v_listen = caml_alloc_custom(&listener_ops, sizeof(nw_listener_t), 0, 1);
  Listener_val(v_listen) = listen;
  CAMLreturn(v_listen);
}

value ocaml_network_listener_create_with_port(value v_port, value v_params) {
  CAMLparam2(v_port, v_params);
  nw_parameters_t params = Params_val(v_params);
  nw_listener_t listen = nw_listener_create_with_port(String_val(v_port), params);
  CAMLlocal1(v_listen);
  v_listen = caml_alloc_custom(&listener_ops, sizeof(nw_listener_t), 0, 1);
  Listener_val(v_listen) = listen;
  CAMLreturn(v_listen);
}

value ocaml_network_listener_create_with_connection(value v_conn, value v_params) {
  CAMLparam2(v_conn, v_params);
  nw_parameters_t params = Params_val(v_params);
  nw_listener_t listen = nw_listener_create_with_connection(Connection_val(v_conn), params);
  CAMLlocal1(v_listen);
  v_listen = caml_alloc_custom(&listener_ops, sizeof(nw_listener_t), 0, 1);
  Listener_val(v_listen) = listen;
  CAMLreturn(v_listen);
}

// ~~~Parameters~~~
static struct custom_operations parameters_ops = {
    "network.parameters",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_network_parameters_create(value v_unit) {
  CAMLparam1(v_unit);
  nw_parameters_t params = nw_parameters_create();
  CAMLlocal1(v_params);
  v_params = caml_alloc_custom(&parameters_ops, sizeof(nw_parameters_t), 0, 1);
  Params_val(v_params) = params;
  CAMLreturn(v_params);
}
