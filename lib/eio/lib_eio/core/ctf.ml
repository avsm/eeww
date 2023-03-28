(* Copyright (C) 2014, Thomas Leonard *)

(* Note: we expect some kind of logger to process the trace buffer to collect
events, but currently we don't have any barriers to ensure that the buffer
is in a consistent state (although it usually is). So for now, you should
pause tracing before trying to parse the buffer. In particular, GC events
complicate things because we may need to add a GC event while in the middle
of adding some other event. *)

open Bigarray

module BS = struct
  (* Replacement for endianBigstring that avoids pulling in a Unix dependency *)

  external set_64 : Cstruct.buffer -> int -> int64 -> unit = "%caml_bigstring_set64"
  external swap64 : int64 -> int64 = "%bswap_int64"
  external unsafe_chr : int -> char = "%identity"

  let set_int8 s off v = Array1.set s off (unsafe_chr v)
  [@@ocaml.inline]

  let set_int64_le s off v =
    if Sys.big_endian
    then set_64 s off (swap64 v)
    else set_64 s off v
  [@@ocaml.inline]
end

type id = int

let last_id = ref 0

let mint_id () =
  incr last_id;
  !last_id

type hiatus_reason =
  | Wait_for_work
  | Suspend
  | Hibernate

type event =
  | Wait
  | Task
  | Bind
  | Try
  | Choose
  | Pick
  | Join
  | Map
  | Condition
  | On_success
  | On_failure
  | On_termination
  | On_any
  | Ignore_result
  | Async
  | Promise
  | Semaphore
  | Switch
  | Stream
  | Mutex

type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

let current_thread = ref (-1)

let int_of_thread_type t =
  match t with
  | Wait -> 0
  | Task -> 1
  | Bind -> 2
  | Try -> 3
  | Choose -> 4
  | Pick -> 5
  | Join -> 6
  | Map -> 7
  | Condition -> 8
  | On_success -> 9
  | On_failure -> 10
  | On_termination -> 11
  | On_any -> 12
  | Ignore_result -> 13
  | Async -> 14
  | Promise -> 15
  | Semaphore -> 16
  | Switch -> 17
  | Stream -> 18
  | Mutex -> 19

let event_to_string (t : event) =
  match t with
  | Wait -> "wait"
  | Task -> "task"
  | Bind -> "bind"
  | Try -> "try"
  | Choose -> "choose"
  | Pick -> "pick"
  | Join -> "join"
  | Map -> "map"
  | Condition -> "condition"
  | On_success -> "on-success"
  | On_failure -> "on-failure"
  | On_termination -> "on-termination"
  | On_any -> "on-any"
  | Ignore_result -> "ignore-result"
  | Async -> "async"
  | Promise -> "promise"
  | Semaphore -> "semaphore"
  | Switch -> "switch"
  | Stream -> "stream"
  | Mutex -> "mutex"

let int_to_thread_type = function
  | 0 -> Wait
  | 1 -> Task
  | 2 -> Bind
  | 3 -> Try
  | 4 -> Choose
  | 5 -> Pick
  | 6 -> Join
  | 7 -> Map
  | 8 -> Condition
  | 9 -> On_success
  | 10 -> On_failure
  | 11 -> On_termination
  | 12 -> On_any
  | 13 -> Ignore_result
  | 14 -> Async
  | 15 -> Promise
  | 16 -> Semaphore
  | 17 -> Switch
  | 18 -> Stream
  | 19 -> Mutex
  | _ -> assert false

type Runtime_events.User.tag += Created

let created_type =
  let encode buf ((child : int), (thread_type : event)) =
    Bytes.set_int32_le buf 0 (Int32.of_int child);
    Bytes.set_int8 buf 4 (int_of_thread_type thread_type);
    5
  in
  let decode buf _size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int in
    let thread_type = Bytes.get_int8 buf 4 |> int_to_thread_type in
    (child, thread_type)
  in
  Runtime_events.Type.register ~encode ~decode

let created = Runtime_events.User.register "eio.created" Created created_type

let two_ids_type =
  let encode buf ((child : int), i) =
    Bytes.set_int32_le buf 0 (Int32.of_int i);
    Bytes.set_int8 buf 4 i;
    5
  in
  let decode buf _size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int  in
    let thread_type = Bytes.get_int8 buf 4 in
    (child, thread_type)
  in
  Runtime_events.Type.register ~encode ~decode

type Runtime_events.User.tag += Read
let read = Runtime_events.User.register "eio.read" Read two_ids_type

type Runtime_events.User.tag += Try_read
let try_read = Runtime_events.User.register "eio.try_read" Try_read two_ids_type

type Runtime_events.User.tag += Failed

let labelled_type =
  let encode buf ((child : int), exn) =
    (* Check size of buf and use smallest size which means we may
      have to truncate the label. *)
    let available_buf_len = Bytes.length buf - 1 in
    let exn_len = String.length exn in
    let data_len = min available_buf_len exn_len in
    Bytes.set_int32_le buf 0 (Int32.of_int child);
    Bytes.blit_string exn 0 buf 4 data_len;
    data_len + 4
  in
  let decode buf size =
    let child = Bytes.get_int32_le buf 0 |> Int32.to_int in
    let size = size - 4 in
    let target = Bytes.create size in
    Bytes.blit buf 4 target 0 size;
    (child, Bytes.unsafe_to_string target)
  in
  Runtime_events.Type.register ~encode ~decode
let failed = Runtime_events.User.register "eio.fail" Failed labelled_type
type Runtime_events.User.tag += Resolved
let resolved = Runtime_events.User.(register "eio.resolved" Resolved Runtime_events.Type.int)

type Runtime_events.User.tag += Name
let named = Runtime_events.User.register "eio.name" Name labelled_type

type Runtime_events.User.tag += Loc
let located = Runtime_events.User.register "eio.loc" Loc labelled_type
type Runtime_events.User.tag += Log
let logged = Runtime_events.User.register "eio.log" Log labelled_type

type Runtime_events.User.tag += Switch
type Runtime_events.User.tag += Increase
let increase = Runtime_events.User.register "eio.increase" Increase labelled_type
let switch = Runtime_events.User.register "eio.switch" Switch Runtime_events.Type.int
type Runtime_events.User.tag += Value
let value = Runtime_events.User.register "eio.value" Value labelled_type
type Runtime_events.User.tag += Signal
let signal = Runtime_events.User.register "eio.signal" Signal two_ids_type

type Runtime_events.User.tag += Suspend
let suspend = Runtime_events.User.register "eio.suspend" Suspend Runtime_events.Type.unit

module Control = struct
  (* Following LTT, our trace buffer is divided into a small number of
  * fixed-sized "packets", each of which contains many events. When there
  * isn't room in the current packet for the next event, we move to the next
  * packet. This wastes a few bytes at the end of each packet, but it allows
  * us to discard whole packets at a time when we need to overwrite something.
  *)

  let event_log = ref true

  let stop () =
    match !event_log with
    | true -> event_log := false
    | _ -> failwith "Log is not currently tracing!"

  let add_event = Runtime_events.User.write

  let note_created child thread_type =
    assert ((child :> int) >= 0);
    add_event created (child, thread_type)

  let note_read ~reader input =
    add_event read (reader, input)

  let note_try_read thread input =
    add_event try_read (thread, input)

  let note_signal ~src dst =
    add_event signal (src, dst)

  let note_resolved p ~ex =
    match ex with
    | Some ex ->
        let msg = Printexc.to_string ex in
        add_event failed (p, msg)
    | None ->
        add_event resolved p

  let note_log thread msg =
    add_event logged (thread, msg)

  let note_located thread msg =
    add_event located (thread, msg)

  let note_named thread msg =
    add_event named (thread, msg)

  let note_increase counter amount =
    add_event increase (amount, counter)

  let note_counter_value counter v =
    add_event value (v, counter)

  let note_switch new_current =
    if new_current <> !current_thread then (
      current_thread := new_current;
      add_event switch new_current
    )

  let note_suspend () =
    current_thread := (-1);
    add_event suspend ()

  let start () =
    event_log := true;
    current_thread := -1
end

let log name =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_log !current_thread name

let set_name name =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_named !current_thread name

let set_loc name =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_located !current_thread name

let note_fork () =
  let child = mint_id () in
  begin match !Control.event_log with
    | false -> ()
    | true ->
      Control.note_created child Task
  end;
  child

let note_created ?label ?loc id ty =
  match !Control.event_log with
  | false -> ()
  | true ->
    Control.note_created id ty;
    Option.iter (Control.note_named id) label;
    Option.iter (Control.note_located id) loc

let note_switch new_current =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_switch new_current

let note_hiatus _reason =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_suspend ()

let note_resume new_current =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_switch new_current

let note_try_read input =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_try_read !current_thread input

let note_read ?reader input =
  match !Control.event_log with
  | false -> ()
  | true ->
    let reader =
      match reader with
      | None -> !current_thread
      | Some r -> r
    in
    Control.note_read ~reader input

let note_resolved id ~ex =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_resolved id ~ex

let note_signal ?src dst =
  match !Control.event_log with
  | false -> ()
  | true ->
    let src =
      match src with
      | None -> !current_thread
      | Some x -> x
    in
    Control.note_signal ~src dst

let note_increase counter amount =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_increase counter amount

let note_counter_value counter value =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_counter_value counter value

let should_resolve thread =
  match !Control.event_log with
  | false -> ()
  | true -> Control.note_named thread "__should_resolve" (* Hack! *)

let demango x = List.flatten (
  List.map (fun i ->
    Astring.String.cuts ~sep:"__" i
    |> List.fold_left (fun a b -> match (a, b) with
      | [], b -> [b]
      | v, "" -> v
      | a::v, s when Astring.Char.Ascii.is_lower s.[0] -> (a^"_"^s) :: v
      | v, s -> s :: v) []
    |> List.rev ) x
  )

let is_outer raw_entry =
  let slot = Printexc.backtrace_slots_of_raw_entry raw_entry in
  match slot with
  | None -> None
  | Some slots ->
    Array.find_map (fun slot ->
      let (let*) = Option.bind in
      let* loc = Printexc.Slot.location slot in
      let* name = Printexc.Slot.name slot in
      let* name = match String.split_on_char '.' name |> demango with
        | "Eio_core" :: _ -> None
        | "Eio" :: _ -> None
        | "Eio_linux" :: _ -> None
        | "Eio_luv" :: _ -> None
        | "Eio_main" :: _ -> None
        | "Stdlib" :: _ -> None
        | "Dune_exe" :: v -> Some (String.concat "." v)
        | v -> Some (String.concat "." v)
      in
      Some (Fmt.str "%s (%s:%d)" name loc.filename loc.line_number)
    ) slots
  | Some _ -> None

let dune_exe_strategy stack =
  let first acc s = match acc with
    | (Some _ as v) -> v
    | _ -> is_outer s
  in
  List.fold_left first None stack

let get_caller () =
  let p = Printexc.get_callstack 30 |> Printexc.raw_backtrace_to_string in
  let stack = Printexc.get_callstack 30 |> Printexc.raw_backtrace_entries |> Array.to_list in
  match dune_exe_strategy stack with
  | Some v -> v
  | None -> p