(* A wrapper around the low-level Kqueue bindings. This allows
   users to use the ever-useful Heap to associate data with an
   event and retrieve it later when the even is ready. *)

type 'a t = {
  kq : Kqueue.t;
  heap : 'a Heap.t;
}

type 'a job = 'a Heap.entry

let check t =
  if Heap.is_released t.heap then
    invalid_arg "Can't use ring after Uring.exit has been called"

let with_id_full : type a. a t -> (Heap.ptr -> Kqueue.Event_list.Event.t option) -> a -> extra_data:'b -> (Kqueue.Event_list.Event.t * a job) option =
  fun t fn datum ~extra_data ->
  match Heap.alloc t.heap datum ~extra_data with
  | exception (Invalid_argument _ as ex) -> check t; raise ex
  | entry ->
    let ptr = Heap.ptr entry in
    match fn ptr with
    | Some ev -> Some (ev, entry)
    | None ->
      ignore (Heap.free t.heap ptr : a);
      None

let with_id t fn a = with_id_full t fn a ~extra_data:()

module Events = struct
  include Kqueue.Event_list.Event

  let singleton ?flags ?filter ?data ident udata =
    let evs = Kqueue.Event_list.create 1 in
    let ev = Kqueue.Event_list.get evs 0 in
    Option.iter (Kqueue.Event_list.Event.set_flags ev) flags;
    Option.iter (Kqueue.Event_list.Event.set_filter ev) filter;
    Option.iter (Kqueue.Event_list.Event.set_data ev) data;
    Kqueue.Event_list.Event.set_ident ev ident;
    Kqueue.Event_list.Event.set_udata ev udata;
    evs
end

let submit_to_kqueue kq evs =
  let v : int = Kqueue.kevent kq ~changelist:evs ~eventlist:Kqueue.Event_list.null Kqueue.Timeout.immediate in
  assert (v = 0)

let submit ?flags ?filter ?data ?ident t user_data =
  with_id t (fun ptr ->
      let ident = Option.value ~default:(ptr :> int) ident in
      let evs = Events.singleton ?flags ?filter ?data ident (ptr :> int) in
      let ev = Kqueue.Event_list.get evs 0 in
      submit_to_kqueue t.kq evs;
      Some ev) user_data

let cancel ?flags ?filter ?data ~ident t =
  let evs = Events.singleton ?flags ?filter ?data ident 0 in
  submit_to_kqueue t.kq evs

type 'a ready = Kqueue.Event_list.Event.t * 'a

let wait ?(timeout=Kqueue.Timeout.never) ~max_events t =
  let eventlist = Kqueue.Event_list.create max_events in
  let got = Kqueue.kevent t.kq ~changelist:Kqueue.Event_list.null ~eventlist timeout in
  List.init got (fun i ->
      let ev = Kqueue.Event_list.get eventlist i in
      let ptr = Kqueue.Event_list.Event.get_udata ev in
      let data = Heap.free t.heap ptr in
      (ev, data)
    )

module Ident = struct
  type t = int
  let to_fd = Kqueue.Util.file_descr_of_int
  let of_fd = Kqueue.Util.file_descr_to_int
end

let create () =
  let kq = Kqueue.create () in
  let heap = Heap.create 64 in
  { kq; heap }

let close t =
  Kqueue.close t.kq;
  Heap.release t.heap