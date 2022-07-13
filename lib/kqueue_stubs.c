#include "config.h"

#ifdef KQUEUE_AVAILABLE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/types.h>

#define Kqueue_constant(name, i)                                               \
    CAMLprim value name(value unit) { return Val_int(i); }

  CAMLprim value kqueue_ml_kqueue_create(value unit) {
    CAMLparam1(unit);
    int k;
    k = kqueue();
    if (k == -1)
      uerror("kqueue", Nothing);
    fcntl(k, F_SETFD, FD_CLOEXEC);
    CAMLreturn(Val_long(k));
  }

  CAMLprim value kqueue_ml_kevent_sizeof(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_long(sizeof(struct kevent)));
  }

  CAMLprim value kqueue_ml_kevent_offset_event_fd(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, ident)));
  }

  CAMLprim value kqueue_ml_kevent_offset_filter(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, filter)));
  }

  CAMLprim value kqueue_ml_kevent_offset_flags(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, flags)));
  }

  CAMLprim value kqueue_ml_kevent_offset_fflags(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, fflags)));
  }

  CAMLprim value kqueue_ml_kevent_offset_data(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, data)));
  }

  CAMLprim value kqueue_ml_kevent_offset_udata(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(offsetof(struct kevent, udata)));
  }

  CAMLprim value kqueue_ml_kevent(value kqueue_fd, value changelist, value eventlist, value timeout) {
    CAMLparam4(kqueue_fd, changelist, eventlist, timeout);
    struct kevent * changes;
    struct kevent * events;
    int ret, event_count, change_count;
    int64_t ns;
    ns = Int64_val(timeout);
    changes = (struct kevent *) Caml_ba_data_val(changelist);
    events = (struct kevent *) Caml_ba_data_val(eventlist);
    event_count = Caml_ba_array_val(eventlist)->dim[0] / sizeof (struct kevent);
    change_count = Caml_ba_array_val(changelist)->dim[0] / sizeof (struct kevent);
    if (ns == 0) {
      struct timespec t = { 0, 0 };
      ret = kevent(Long_val(kqueue_fd), changes, change_count, events, event_count, &t);
    } else if (ns < 0) {
      caml_enter_blocking_section();
      ret = kevent(Long_val(kqueue_fd), changes, change_count, events, event_count, NULL);
      caml_leave_blocking_section();
    } else {
      struct timespec t;
      t.tv_sec = ns / 1000000000;
      t.tv_nsec = (ns % 1000000000);
      caml_enter_blocking_section();
      ret = kevent(Long_val(kqueue_fd), changes, change_count, events, event_count, &t);
      caml_leave_blocking_section();
    }
    if (ret == -1)
      uerror("kevent", Nothing);
    CAMLreturn(Val_long(ret));
  }

  #ifdef EVFILT_USER_AVAILABLE
    Kqueue_constant(kqueue_filter_evfilt_user, EVFILT_USER)
    Kqueue_constant(kqueue_note_ffnop, NOTE_FFNOP)
    Kqueue_constant(kqueue_note_ffand, NOTE_FFAND)
    Kqueue_constant(kqueue_note_ffor, NOTE_FFOR)
    Kqueue_constant(kqueue_note_ffcopy, NOTE_FFCOPY)
    Kqueue_constant(kqueue_note_ffctrlmask, NOTE_FFCTRLMASK)
    Kqueue_constant(kqueue_note_fflagsmask, NOTE_FFLAGSMASK)
    Kqueue_constant(kqueue_note_trigger, NOTE_TRIGGER)
  #endif 

  Kqueue_constant(kqueue_filter_evfilt_read, EVFILT_READ)
  Kqueue_constant(kqueue_filter_evfilt_write, EVFILT_WRITE)
  Kqueue_constant(kqueue_filter_evfilt_timer, EVFILT_TIMER)
  Kqueue_constant(kqueue_filter_evfilt_vnode, EVFILT_VNODE)
  Kqueue_constant(kqueue_filter_evfilt_proc, EVFILT_PROC)

  Kqueue_constant(kqueue_flag_ev_add, EV_ADD)
  Kqueue_constant(kqueue_flag_ev_receipt, EV_RECEIPT)
  Kqueue_constant(kqueue_flag_ev_enable, EV_ENABLE)
  Kqueue_constant(kqueue_flag_ev_disable, EV_DISABLE)
  Kqueue_constant(kqueue_flag_ev_delete, EV_DELETE)
  Kqueue_constant(kqueue_flag_ev_oneshot, EV_ONESHOT)
  Kqueue_constant(kqueue_flag_ev_clear, EV_CLEAR)
  Kqueue_constant(kqueue_flag_ev_eof, EV_EOF)
  Kqueue_constant(kqueue_flag_ev_error, EV_ERROR)
  Kqueue_constant(kqueue_note_seconds, NOTE_SECONDS)
  Kqueue_constant(kqueue_note_useconds, NOTE_USECONDS)
  Kqueue_constant(kqueue_note_nseconds, NOTE_NSECONDS)
  Kqueue_constant(kqueue_note_lowat, NOTE_LOWAT)
  Kqueue_constant(kqueue_note_oob, NOTE_OOB)
  Kqueue_constant(kqueue_note_delete, NOTE_DELETE)
  Kqueue_constant(kqueue_note_write, NOTE_WRITE)
  Kqueue_constant(kqueue_note_extend, NOTE_EXTEND)
  Kqueue_constant(kqueue_note_attrib, NOTE_ATTRIB)
  Kqueue_constant(kqueue_note_link, NOTE_LINK)
  Kqueue_constant(kqueue_note_rename, NOTE_RENAME)
  Kqueue_constant(kqueue_note_revoke, NOTE_REVOKE)
  Kqueue_constant(kqueue_note_exit, NOTE_EXIT)
  Kqueue_constant(kqueue_note_fork, NOTE_FORK)
  Kqueue_constant(kqueue_note_exec, NOTE_EXEC)
  Kqueue_constant(kqueue_note_signal, NOTE_SIGNAL)
#else
  typedef int dummy_definition;
#endif
