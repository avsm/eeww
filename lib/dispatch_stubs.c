#include <dispatch/dispatch.h>
#include <block.h>
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
#define Block_val(v) (*((dispatch_block_t *)Data_custom_val(v)))

value *make_callback(value v_fun)
{
  value *m_fun = (value *)caml_stat_alloc(sizeof(v_fun));
  *m_fun = v_fun;
  caml_register_generational_global_root(&(*m_fun));
  return m_fun;
}

void free_callback(value *v_fun)
{
  caml_remove_generational_global_root(&(*v_fun));
  caml_stat_free(v_fun);
}

value value_of_bool(bool b) {
  CAMLparam0();
  if (b) {
    CAMLreturn(3);
  } else {
    CAMLreturn(1);
  }
}

// ~~~Functions~~~
value ocaml_dispatch_main(value v_unit) {
  CAMLparam1(v_unit);
  // Won't return
  dispatch_main();
  CAMLreturn(Val_unit);
}

// ~~~Block~~~
static struct custom_operations block_ops = {
    "dispatch.block",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

// TODO: not convinced by this code... have to unregister callback!
value ocaml_dispatch_block_create(value v_thunk) {
  CAMLparam1(v_thunk);
  CAMLlocal1(v_block);
  value *m_thunk = make_callback(v_thunk);
  dispatch_block_t block = ^{
    caml_callback(*m_thunk, Val_unit);
  };
  v_block = caml_alloc_custom(&block_ops, sizeof(dispatch_block_t), 0, 1);
  Block_val(v_block) = Block_copy(block);
  CAMLreturn(v_block);
}

value ocaml_dispatch_block_destroy(value v_block) {
  CAMLparam1(v_block);

  dispatch_block_t block = Block_val(v_block);
  Block_release(block);

  CAMLreturn(Val_unit);
}

value ocaml_dispatch_block_exec(value v_block) {
  CAMLparam1(v_block);

  dispatch_block_t block = Block_val(v_block);
  block();
  
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_async(value v_queue, value v_fun)
{
  CAMLparam2(v_queue, v_fun);
  value *m_fun = make_callback(v_fun);
  dispatch_queue_t queue = Queue_val(v_queue);
  // caml_release_runtime_system(); not sure why this doesn't need to be called?
  dispatch_async(queue, ^{
    int res = caml_c_thread_register();
    if (res)
      caml_acquire_runtime_system();

    caml_callback(*m_fun, Val_unit);
    free_callback(m_fun);

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_sync(value v_queue, value v_fun)
{
  CAMLparam2(v_queue, v_fun);
  value *m_fun = make_callback(v_fun);
  dispatch_queue_t queue = Queue_val(v_queue);
  caml_release_runtime_system();
  dispatch_sync(queue, ^{
    int res = caml_c_thread_register();

    if (res)
      caml_acquire_runtime_system();

    caml_callback(*m_fun, Val_unit);

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

// ~~~ Queues ~~~
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

value ocaml_dispath_queue_finalise(value v_queue)
{
  CAMLparam1(v_queue);
  dispatch_queue_t q = Queue_val(v_queue);
  if (q)
  {
    dispatch_release(q);
    caml_remove_global_root(&v_queue);
    Queue_val(v_queue) = NULL;
  }
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_get_main_queue(value v_unit) {
  CAMLparam1(v_unit);
  CAMLlocal1(v_queue);
  dispatch_queue_t queue = dispatch_get_main_queue();
  v_queue = caml_alloc_custom(&queue_ops, sizeof(dispatch_queue_t), 0, 1);
  Queue_val(v_queue) = queue;
  CAMLreturn(v_queue);
}

value ocaml_dispatch_get_global_queue(value v_qos) {
  CAMLparam1(v_qos);
  CAMLlocal1(v_queue);
  intptr_t ident;

  if (v_qos == 1) {
    ident = QOS_CLASS_USER_INTERACTIVE;
  } else if (v_qos == 3) {
    ident = QOS_CLASS_USER_INITIATED;
  } else if (v_qos == 5) {
    ident = QOS_CLASS_UTILITY;
  } else {  
    ident = QOS_CLASS_BACKGROUND;
  }

  dispatch_queue_t queue = dispatch_get_global_queue(ident, 0);
  v_queue = caml_alloc_custom(&queue_ops, sizeof(dispatch_queue_t), 0, 1);
  Queue_val(v_queue) = queue;
  CAMLreturn(v_queue);
}

value ocaml_dispatch_queue_create(value v_typ, value v_unit)
{
  CAMLparam2(v_typ, v_unit);
  CAMLlocal1(v_queue);
  dispatch_queue_t queue;

  if (Val_int(0) == v_typ)
  {
    // Serial queue
    queue = dispatch_queue_create("caml.queue.create.serial", DISPATCH_QUEUE_SERIAL);
  }
  else
  {
    // Concurrent queue
    queue = dispatch_queue_create("caml.queue.create.concurrent", DISPATCH_QUEUE_CONCURRENT);
  }

  // So we control the releasing of the queue
  dispatch_retain(queue);
  v_queue = caml_alloc_custom(&queue_ops, sizeof(dispatch_queue_t), 0, 1);
  caml_register_global_root(&v_queue); // Might not be necessary...
  Queue_val(v_queue) = queue;
  CAMLreturn(v_queue);
}

// ~~~ Time ~~~

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

// ~~~ Groups ~~~

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
  dispatch_retain(group);
  value v_group = caml_alloc_custom(&queue_ops, sizeof(dispatch_group_t), 0, 1);
  Group_val(v_group) = group;
  CAMLreturn(v_group);
}

value ocaml_dispatch_group_wait(value v_group, value v_time)
{
  CAMLparam2(v_group, v_time);
  CAMLlocal1(v_res);
  value *g = caml_stat_alloc(sizeof(dispatch_group_t));
  *g = v_group;
  caml_register_generational_global_root(&(*g));
  value *t = caml_stat_alloc(sizeof(dispatch_group_t));
  *t = v_time;
  caml_register_generational_global_root(&(*t));

  caml_release_runtime_system();
  int v = dispatch_group_wait(Group_val(*g), Val_int(*t));
  if (v != 0)
  {
    v_res = 3;
  }
  else
  {
    v_res = 1;
  }

  caml_remove_generational_global_root(g);
  caml_remove_generational_global_root(t);
  caml_stat_free(g);
  caml_stat_free(t);
  caml_acquire_runtime_system();
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

// ~~~ Data ~~~

value ocaml_dispatch_data_create(value v_ba)
{
  CAMLparam1(v_ba);
  CAMLlocal1(v_empty);
  dispatch_queue_t queue = dispatch_get_global_queue(
      DISPATCH_QUEUE_PRIORITY_BACKGROUND, 0);
  dispatch_data_t empty = dispatch_data_create(Caml_ba_data_val(v_ba), (Caml_ba_array_val(v_ba))->dim[0], queue, DISPATCH_DATA_DESTRUCTOR_DEFAULT);

  v_empty = caml_alloc_custom(&queue_ops, sizeof(dispatch_data_t), 0, 1);
  Data_val(v_empty) = empty;

  CAMLreturn(v_empty);
}

value ocaml_dispatch_data_empty(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(v_empty);
  dispatch_data_t empty = dispatch_data_empty;
  v_empty = caml_alloc_custom(&queue_ops, sizeof(dispatch_data_t), 0, 1);
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
  value *m_f = make_callback(v_f);
  dispatch_data_t data = Data_val(v_data);
  dispatch_data_apply(data, ^(dispatch_data_t region, size_t offset, const void *buffer, size_t size) {
    value d = caml_alloc_custom(&queue_ops, sizeof(dispatch_data_t), 0, 1);
    Data_val(d) = region;
    caml_callback_exn(*m_f, d);
    free_callback(m_f);
    return (_Bool) true;
  });
  CAMLreturn(Val_unit);
}

// ~~ Input/Output ~~~

static struct custom_operations io_ops = {
    "dispatch.io",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

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

  v_channel = caml_alloc_custom(&io_ops, sizeof(dispatch_io_t), 0, 1);
  Channel_val(v_channel) = channel;

  CAMLreturn(v_channel);
}

value ocaml_dispatch_io_handler(value *handler, bool done, dispatch_data_t data, int error) {
  CAMLparam0();
  CAMLlocal1(v_data);
  v_data = caml_alloc_custom(&queue_ops, sizeof(dispatch_data_t), 0, 1);
  Data_val(v_data) = data;
  caml_callback3(*handler, Val_int(error), value_of_bool(done), v_data);
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_with_read(value v_channel, value v_off, value v_len, value v_queue, value v_handler)
{
  CAMLparam5(v_queue, v_off, v_len, v_channel, v_handler);
  // Allocate everything the thread is going to need
  value *m_handler = make_callback(v_handler);
  dispatch_queue_t queue = Queue_val(v_queue);

  dispatch_io_read(Channel_val(v_channel), Val_int(v_off), Val_int(v_len), queue, ^(bool done, dispatch_data_t data, int error) {
    int res = caml_c_thread_register();

    if (res)
      caml_acquire_runtime_system();
    
    if (data != NULL) 
      ocaml_dispatch_io_handler(m_handler, done, data, error);

    if (done) {
      free_callback(m_handler);
    }

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

value ocaml_dispatch_with_write(value v_channel, value v_off, value v_data, value v_queue, value v_handler)
{
  CAMLparam5(v_queue, v_channel, v_off, v_data, v_handler);
  // Allocate everything the thread is going to need
  value *m_handler = make_callback(v_handler);
  dispatch_queue_t queue = Queue_val(v_queue);

  dispatch_io_write(Channel_val(v_channel), Val_int(v_off), Data_val(v_data), queue, ^(bool done, dispatch_data_t data, int error) {
    int res = caml_c_thread_register();

    if (res)
      caml_acquire_runtime_system();

    ocaml_dispatch_io_handler(m_handler, done, data, error);

    if (done)
    {
      free_callback(m_handler);
    }

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
    }
    return;
  });
  CAMLreturn(Val_unit);
}

// value ocaml_dispatch_read_bytecode(value *argv, int argn)
// {
//   return ocaml_dispatch_read(argv[0], argv[1], argv[2], argv[3],
//                              argv[4], argv[5]);
// }

value ocaml_dispatch_write(value v_queue, value v_channel, value v_offset, value v_g, value v_d)
{
  CAMLparam5(v_queue, v_g, v_channel, v_offset, v_d);
  dispatch_queue_t queue = Queue_val(v_queue);
  dispatch_group_t g = Group_val(v_g);

  dispatch_group_enter(g);
  dispatch_io_write(Channel_val(v_channel), Int_val(v_offset), Data_val(v_d), queue, ^(bool done, dispatch_data_t data, int error) {
    int res = caml_c_thread_register();
    if (res)
      caml_acquire_runtime_system();

    if (error)
    {
      printf("Error(%i)\n", error);
      if (res)
      {
        caml_release_runtime_system();
        caml_c_thread_unregister();
      }
      return;
    }

    if (done)
    {
      dispatch_group_leave(g);
    }

    if (res)
    {
      caml_release_runtime_system();
      caml_c_thread_unregister();
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
