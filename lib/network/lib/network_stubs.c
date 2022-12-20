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
#include <caml/socketaddr.h>
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
#define Endpoint_val(v) (*((nw_endpoint_t *)Data_custom_val(v)))

value error_to_value(nw_error_t error) {
  if (error) {
    return Val_int(nw_error_get_error_code(error));
  }
  return Val_int(0);
}

// ~~~Endpoints~~~
static struct custom_operations endpoint_ops = {
    "network.endpoint",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_network_endpoint_create_host(value v_hostname, value v_port) {
  CAMLparam2(v_hostname, v_port);
  CAMLlocal1(v_endpoint);
  nw_endpoint_t endpoint = nw_endpoint_create_host(String_val(v_hostname), String_val(v_port));
  v_endpoint = caml_alloc_custom(&endpoint_ops, sizeof(nw_endpoint_t), 0, 1);
  Endpoint_val(v_endpoint) = endpoint;
  CAMLreturn(v_endpoint);
}

value ocaml_network_endpoint_create_address(value v_sockaddr) {
  CAMLparam1(v_sockaddr);
  CAMLlocal1(v_endpoint);
  union sock_addr_union addr;
  socklen_param_type addr_len;
  get_sockaddr(v_sockaddr, &addr, &addr_len);
  nw_endpoint_t endpoint = nw_endpoint_create_address(&addr.s_gen);
  v_endpoint = caml_alloc_custom(&endpoint_ops, sizeof(nw_endpoint_t), 0, 1);
  Endpoint_val(v_endpoint) = endpoint;
  CAMLreturn(v_endpoint);
}

value ocaml_network_endpoint_release(value v_endpoint) {
  CAMLparam1(v_endpoint);
  nw_release(Endpoint_val(v_endpoint));
  CAMLreturn(Val_unit);
}

// Is this correct?
value ocaml_network_endpoint_get_address(value v_endpoint) {
  CAMLparam1(v_endpoint);
  union sock_addr_union addr;
  nw_endpoint_t endpoint = Endpoint_val(v_endpoint);
  const struct sockaddr *sockaddr = nw_endpoint_get_address(endpoint);
  socklen_param_type addr_len = sizeof(sockaddr);
  memcpy(&addr.s_gen, sockaddr, addr_len);
  return alloc_sockaddr(&addr, addr_len, -1);
}

value ocaml_network_endpoint_get_type(value v_endpoint) {
  CAMLparam1(v_endpoint);
  nw_endpoint_t endpoint = Endpoint_val(v_endpoint);
  nw_endpoint_type_t type = nw_endpoint_get_type(endpoint);
  CAMLreturn(Val_int(type));
}

// ~~~Connection~~~
static struct custom_operations connection_ops = {
    "network.connection",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_network_connection_create(value v_endpoint, value v_params) {
  CAMLparam2(v_endpoint, v_params);
  CAMLlocal1(v_connection);
  nw_connection_t connection = nw_connection_create(Endpoint_val(v_endpoint), Params_val(v_params));
  v_connection = caml_alloc_custom(&connection_ops, sizeof(nw_connection_t), 0, 1);
  Connection_val(v_connection) = connection;
  CAMLreturn(v_connection);
}

value ocaml_network_connection_set_queue(value v_queue, value v_connection) {
  CAMLparam2(v_queue, v_connection);
  nw_connection_set_queue(Connection_val(v_connection), Queue_val(v_queue));
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_retain(value v_connection) {
  CAMLparam1(v_connection);
  nw_retain(Connection_val(v_connection));
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_start(value v_connection) {
  CAMLparam1(v_connection);
  nw_connection_start(Connection_val(v_connection));
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_restart(value v_connection) {
  CAMLparam1(v_connection);
  nw_connection_restart(Connection_val(v_connection));
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_cancel(value v_connection) {
  CAMLparam1(v_connection);
  nw_connection_cancel(Connection_val(v_connection));
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_copy_endpoint(value v_connection) {
  CAMLparam1(v_connection);
  CAMLlocal1(v_endpoint);
  nw_endpoint_t endpoint = nw_connection_copy_endpoint(Connection_val(v_connection));
  v_endpoint = caml_alloc_custom(&endpoint_ops, sizeof(nw_endpoint_t), 0, 1);
  Endpoint_val(v_endpoint) = endpoint;
  CAMLreturn(v_endpoint);
}

value state_to_value(nw_connection_state_t state) {
  // Think this is correct...
  return (Val_int(state));
}

value ocaml_network_connection_set_state_changed_handler(value v_handler, value v_connection) {
  CAMLparam1(v_connection);
  nw_connection_set_state_changed_handler(Connection_val(v_connection), ^(nw_connection_state_t state, nw_error_t error) {
    int res = caml_c_thread_register();
    if (res)
      caml_acquire_runtime_system();

    caml_callback2(v_handler, state_to_value(state), error_to_value(error));

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

value ocaml_network_connection_receive(value v_min, value v_max, value v_comp, value v_conn) {
  CAMLparam4(v_min, v_max, v_comp, v_conn);
  nw_connection_receive(Connection_val(v_conn), 1, UINT32_MAX, 
    ^(dispatch_data_t data, nw_content_context_t context, bool is_complete, nw_error_t receive_error) {
      int res = caml_c_thread_register();
      if (res)
        caml_acquire_runtime_system();

      CAMLlocal1(v_data_opt);

      // Is this right? 
      if (data) {
        CAMLlocal1(v_data);
        v_data = caml_alloc_custom(&connection_ops, sizeof(dispatch_data_t), 0, 1);
        Data_val(v_data) = data;
        v_data_opt = caml_alloc_some(v_data);
      } else {
        v_data_opt = Val_none;
      }

      value args[4];
      args[0] = v_data_opt;
      args[1] = Val_unit; // TODO: Pass context
      args[2] = Val_bool(is_complete);
      args[3] = error_to_value(receive_error);
      caml_callbackN(v_comp, 4, args);
      
      if (res)
      {
        caml_release_runtime_system();
        caml_c_thread_unregister();
      }
  });
  CAMLreturn(Val_unit);
}

nw_content_context_t content_context_of_value(value v_int) {
  int i = Int_val(v_int);
  if (v_int == 1) {
    return NW_CONNECTION_FINAL_MESSAGE_CONTEXT;
  }

  return NW_CONNECTION_DEFAULT_MESSAGE_CONTEXT;
}

value ocaml_network_connection_send(value v_d, value v_ctx, value v_bool, value v_comp, value v_conn) {
  CAMLparam5(v_d, v_ctx, v_bool, v_comp, v_conn);
  nw_connection_send(Connection_val(v_conn), Data_val(v_d), content_context_of_value(v_ctx), Bool_val(v_bool),
    ^(nw_error_t receive_error) {
      int res = caml_c_thread_register();
      if (res)
        caml_acquire_runtime_system();

      caml_callback(v_comp, error_to_value(receive_error)); // pass err
      if (res)
      {
        caml_release_runtime_system();
        caml_c_thread_unregister();
      }
      return;
    });
  CAMLreturn(Val_unit);
}

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
  if (listen == NULL) {
    caml_failwith("Network Listener failed to be created");
  }
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

value ocaml_network_listener_get_port(value v_listener) {
  CAMLparam1(v_listener);
  nw_listener_t listener = Listener_val(v_listener);
  uint16_t port = nw_listener_get_port(listener);
  CAMLreturn(Val_int(port));
}

value ocaml_network_listener_start(value v_listener) {
  CAMLparam1(v_listener);
  nw_listener_t listener = Listener_val(v_listener);
  nw_listener_start(listener);
  CAMLreturn(Val_unit);
}

value ocaml_network_listener_retain(value v_listener) {
  CAMLparam1(v_listener);
  nw_listener_t listener = Listener_val(v_listener);
  nw_retain(listener);
  CAMLreturn(Val_unit);
}

value ocaml_network_listener_release(value v_listener) {
  CAMLparam1(v_listener);
  nw_listener_t listener = Listener_val(v_listener);
  nw_release(listener);
  CAMLreturn(Val_unit);
}

value ocaml_network_listener_set_queue(value v_queue, value v_listener) {
  CAMLparam2(v_queue, v_listener);
  nw_listener_set_queue(Listener_val(v_listener), Queue_val(v_queue));
  CAMLreturn(Val_unit);
}

value ocaml_network_listener_cancel(value v_listener) {
  CAMLparam1(v_listener);
  nw_listener_cancel(Listener_val(v_listener));
  CAMLreturn(Val_unit);
}

value listener_state_to_value(nw_listener_state_t state) {
  // Think this is correct...
  return (Val_int(state));
}

value ocaml_network_listener_set_state_changed_handler(value v_handler, value v_listener) {
  CAMLparam2(v_handler, v_listener);
  nw_listener_set_state_changed_handler(Listener_val(v_listener), ^(nw_listener_state_t state, nw_error_t error) {
    int res = caml_c_thread_register();
    if (res)
      caml_acquire_runtime_system();

    caml_callback2(v_handler, listener_state_to_value(state), error_to_value(error));

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

value ocaml_network_listener_set_new_connection_handler(value v_handler, value v_listener) {
  CAMLparam2(v_handler, v_listener);
  nw_listener_set_new_connection_handler(Listener_val(v_listener), ^(nw_connection_t connection) {
    int res = caml_c_thread_register();
    if (res)
      caml_acquire_runtime_system();

    CAMLlocal1(v_connection);
    v_connection = caml_alloc_custom(&connection_ops, sizeof(nw_connection_t), 0, 1);
    Connection_val(v_connection) = connection;
    caml_callback_exn(v_handler, v_connection);

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
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

value ocaml_network_parameters_create_tcp(value v_unit) {
  CAMLparam1(v_unit);
	nw_parameters_configure_protocol_block_t configure_tls = NW_PARAMETERS_DISABLE_PROTOCOL;
  nw_parameters_t params = nw_parameters_create_secure_tcp(configure_tls, NW_PARAMETERS_DEFAULT_CONFIGURATION);
  CAMLlocal1(v_params);
  v_params = caml_alloc_custom(&parameters_ops, sizeof(nw_parameters_t), 0, 1);
  Params_val(v_params) = params;
  nw_retain(params);
  CAMLreturn(v_params);
}

value ocaml_network_parameters_set_local_endpoint(value v_params, value v_endpoint) {
  CAMLparam2(v_params, v_endpoint);
  nw_parameters_set_local_endpoint(Params_val(v_params), Endpoint_val(v_endpoint));
  CAMLreturn(Val_unit);
}

value ocaml_network_parameters_set_reuse_local_address(value v_params, value v_bool) {
  CAMLparam2(v_params, v_bool);
  nw_parameters_set_reuse_local_address(Params_val(v_params), Bool_val(v_bool));
  CAMLreturn(Val_unit);
}


