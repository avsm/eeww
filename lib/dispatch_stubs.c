#include <dispatch/dispatch.h>
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

#define Queue_val(v) (*((struct dispatch_queue_s **)Data_custom_val(v)))
#define Channel_val(v) (*((struct dispatch_io_s **)Data_custom_val(v)))
#define Group_val(v) (*((struct dispatch_group_s **)Data_custom_val(v)))
#define Data_val(v) (*((struct dispatch_data_s **)Data_custom_val(v)))

// Main functions
value ocaml_dispatch_async(value v_queue, value v_fun)
{
  CAMLparam2(v_queue, v_fun);
  dispatch_queue_t queue = Queue_val(v_queue);
  dispatch_async(queue, ^{
    caml_acquire_runtime_system();
    caml_callback(v_fun, Val_unit);
    caml_release_runtime_system();
  });
  return (Val_unit);
}

value ocaml_dispatch_sync(value v_queue, value v_fun)
{
  CAMLparam2(v_queue, v_fun);
  dispatch_queue_t queue = Queue_val(v_queue);
  dispatch_sync(queue, ^{
    caml_callback(v_fun, Val_unit);
  });
  return (Val_unit);
}

// Time

value ocaml_dispatch_now(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Int_val(DISPATCH_TIME_NOW));
}

value ocaml_dispatch_forever(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Int_val(DISPATCH_TIME_FOREVER));
}

// Groups

static struct custom_operations group_ops = {
    "dispatch.group",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_dispatch_group_create(value unit)
{
  CAMLparam1(unit);
  dispatch_group_t group = dispatch_group_create();

  value v_group = caml_alloc_custom_mem(&group_ops, sizeof(dispatch_group_t *), sizeof(dispatch_group_t));
  Group_val(v_group) = group;

  CAMLreturn(v_group);
}

value ocaml_dispatch_group_wait(value v_group, value v_time)
{
  CAMLparam2(v_group, v_time);
  CAMLlocal1(v_res);
  int v = dispatch_group_wait(Group_val(v_group), Val_int(v_time));
  if (v != 0)
  {
    v_res = 3;
  }
  else
  {
    v_res = 1;
  }
  CAMLreturn(v_res);
}

value ocaml_dispatch_group_enter(value v_group)
{
  CAMLparam1(v_group);
  dispatch_group_enter(Group_val(v_group));
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_group_leave(value v_group)
{
  CAMLparam1(v_group);
  dispatch_group_leave(Group_val(v_group));
  CAMLreturn(Val_unit);
}

// Data

value ocaml_dispatch_data_create(value v_ba)
{
  CAMLparam1(v_ba);
  CAMLlocal1(v_empty);
  dispatch_queue_t queue = dispatch_get_global_queue(
      DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0);
  dispatch_data_t empty = dispatch_data_create(Caml_ba_data_val(v_ba), Int_val((Caml_ba_array_val(v_ba))->dim[0]), queue, DISPATCH_DATA_DESTRUCTOR_DEFAULT);

  v_empty = caml_alloc_custom_mem(&group_ops, sizeof(dispatch_data_t *), sizeof(dispatch_data_t));
  Data_val(v_empty) = empty;

  CAMLreturn(v_empty);
}

value ocaml_dispatch_data_empty(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v_empty);
  dispatch_data_t empty = dispatch_data_empty;
  v_empty = caml_alloc_custom_mem(&group_ops, sizeof(dispatch_data_t *), sizeof(dispatch_data_t));
  Data_val(v_empty) = empty;

  CAMLreturn(v_empty);
}

value ocaml_dispatch_data_size(value v_data)
{
  CAMLparam1(v_data);
  CAMLreturn(Val_int(dispatch_data_get_size(Data_val(v_data))));
}

value ocaml_dispatch_data_apply(value v_f, value v_data)
{
  CAMLparam2(v_f, v_data);
  dispatch_data_t data = Data_val(v_data);
  __block value res;
  dispatch_data_apply(data, ^(dispatch_data_t region, size_t offset, const void *buffer, size_t size) {
    value data = caml_alloc_custom_mem(&group_ops, sizeof(dispatch_data_t *), sizeof(dispatch_data_t));
    Data_val(data) = region;
    res = caml_callback_exn(v_f, data);
    // caml_free(data); or somthing...
    return (_Bool) true;
  });
  CAMLreturn(res);
}

// Queues

static struct custom_operations queue_ops = {
    "dispatch.queue",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

value ocaml_dispatch_get_main_queue(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v_queue);
  dispatch_queue_t main_q = dispatch_get_main_queue();

  v_queue = caml_alloc_custom(&queue_ops, sizeof(dispatch_queue_t), 0, 1);
  Queue_val(v_queue) = main_q;

  CAMLreturn(v_queue);
}

value ocaml_dispatch_get_global_queue(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v_queue);
  dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

  v_queue = caml_alloc_custom_mem(&queue_ops, sizeof(dispatch_queue_t *), sizeof(dispatch_queue_t));
  Queue_val(v_queue) = queue;

  CAMLreturn(v_queue);
}

value ocaml_dispatch_queue_create(value typ, value unit)
{
  CAMLparam1(typ);
  CAMLlocal1(v_queue);
  dispatch_queue_t queue = NULL;

  if (Val_int(0) == typ)
  {
    // Serial queue
    queue = dispatch_queue_create("OCaml", DISPATCH_QUEUE_SERIAL);
  }
  else
  {
    // Concurrent queue
    queue = dispatch_queue_create("OCaml", DISPATCH_QUEUE_CONCURRENT);
  }

  v_queue = caml_alloc_custom_mem(&queue_ops, sizeof(dispatch_queue_t *), sizeof(dispatch_queue_t));
  Queue_val(v_queue) = queue;

  CAMLreturn(v_queue);
}

// IO
value ocaml_dispatch_io_create(value v_typ, value v_fd, value v_queue)
{
  CAMLparam3(v_typ, v_fd, v_queue);
  CAMLlocal1(v_channel);
  dispatch_io_type_t typ;

  if (Int_val(v_typ) == 0)
  {
    typ = DISPATCH_IO_STREAM;
  }
  else
  {
    typ = DISPATCH_IO_RANDOM;
  }

  dispatch_fd_t fd = Int_val(v_fd);

  dispatch_io_t channel = dispatch_io_create(typ, fd, Queue_val(v_queue), ^(int error) {
    if (error)
    {
      fputs(strerror(error), stderr);
    }
    close(fd);
  });

  v_channel = caml_alloc_custom_mem(&queue_ops, sizeof(dispatch_io_t *), sizeof(dispatch_io_t));
  Channel_val(v_channel) = channel;

  CAMLreturn(v_channel);
}

value ocaml_dispatch_read(value v_queue, value v_channel, value v_length, value v_offset, value v_g, value v_d)
{
  CAMLparam5(v_queue, v_g, v_channel, v_offset, v_length);
  CAMLxparam1(v_d);
  dispatch_queue_t queue = Queue_val(v_queue);
  __block dispatch_data_t d = dispatch_data_empty;
  dispatch_group_t g = Group_val(v_g);
  dispatch_group_enter(g);
  dispatch_io_read(Channel_val(v_channel), Int_val(v_offset), SIZE_MAX, queue, ^(bool done, dispatch_data_t data, int error) {
    if (error)
      return;

    d = dispatch_data_create_concat(d, data);
    Data_val(v_d) = d;

    if (done)
      dispatch_group_leave(g);

    return;
  });

  CAMLreturn(Val_unit);
}

value ocaml_dispatch_read_bytecode(value *argv, int argn)
{
  return ocaml_dispatch_read(argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5]);
}

value ocaml_dispatch_write(value v_queue, value v_channel, value v_offset, value v_g, value v_d)
{
  CAMLparam5(v_queue, v_g, v_channel, v_offset, v_d);
  dispatch_queue_t queue = Queue_val(v_queue);
  dispatch_data_t d = Data_val(v_d);
  dispatch_group_t g = Group_val(v_g);
  dispatch_group_enter(g);
  dispatch_io_write(Channel_val(v_channel), Int_val(v_offset), d, queue, ^(bool done, dispatch_data_t data, int error) {
    if (error)
    {
      printf("Error(%i)\n", error);
      return;
    }

    if (done)
    {
      dispatch_group_leave(g);
    }

    return;
  });

  CAMLreturn(Val_unit);
}

value ocaml_dispatch_set_high_water(value v_io, value v_value)
{
  CAMLparam2(v_io, v_value);
  dispatch_io_set_high_water(Channel_val(v_io), Int_val(v_value));
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_set_low_water(value v_io, value v_value)
{
  CAMLparam2(v_io, v_value);
  dispatch_io_set_low_water(Channel_val(v_io), Int_val(v_value));
  CAMLreturn(Val_unit);
}