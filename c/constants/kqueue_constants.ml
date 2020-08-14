module Definition (T : Cstubs.Types.TYPE) = struct
  open Ctypes
  open T

  (* flags *)
  let ev_add = constant "EV_ADD" uint16_t

  let ev_enable = constant "EV_ENABLE" uint16_t

  let ev_disable = constant "EV_DISABLE" uint16_t

  let ev_dispatch = constant "EV_DISPATCH" uint16_t

  let ev_delete = constant "EV_DELETE" uint16_t

  let ev_receipt = constant "EV_RECEIPT" uint16_t

  let ev_oneshot = constant "EV_ONESHOT" uint16_t

  let ev_clear = constant "EV_CLEAR" uint16_t

  let ev_eof = constant "EV_EOF" uint16_t

  let ev_error = constant "EV_ERROR" uint16_t

  (* filters *)
  let evfilt_read = constant "EVFILT_READ" short

  let evfilt_write = constant "EVFILT_WRITE" short

  let evfilt_aio = constant "EVFILT_AIO" short

  let evfilt_vnode = constant "EVFILT_VNODE" short

  let evfilt_proc = constant "EVFILT_PROC" short

  let evfilt_signal = constant "EVFILT_SIGNAL" short

  let evfilt_timer = constant "EVFILT_TIMER" short

  let evfilt_user = constant "EVFILT_USER" short

  (* let evfilt_machport = constant "EVFILT_MACHPORT" short *)
  let evfilt_fs = constant "EVFILT_FS" short

  (* let evfilt_vm = constant "EVFILT_VM" short *)
end
