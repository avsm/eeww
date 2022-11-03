module Ctf = Eio.Private.Ctf
let add_callback = Runtime_events.Callbacks.add

let timestamp s = 
  Runtime_events.Timestamp.to_int64 s |> Int64.to_string

let print_events q =
  let evs = Runtime_events.Callbacks.create () in
  let id_event_callback d ts c ((i : Ctf.id), v) = 
    let event s = Queue.push (Fmt.str "[%s:dom%i]%s: %i (%s)\n" (timestamp ts) d s (i :> int) (Ctf.event_to_string v)) q in
    match Runtime_events.User.tag c with
    | Ctf.Created -> event "created"
    | _ -> ()
  in
  let id_string_callback d ts c ((i : Ctf.id), v) = 
    let event s = Queue.push (Fmt.str "[%s:dom%i]%s: %i (%s)\n" (timestamp ts) d s (i :> int) v) q in
    match Runtime_events.User.tag c with
    | Ctf.Label -> event "label"
    | Ctf.Failed -> event "failed"
    | Ctf.Increase -> event "increase"
    | Ctf.Value -> event "value"
    | _ -> ()
  in
  let id_id_callback d ts c ((i, j) : Ctf.id * Ctf.id) =
    let event s = Queue.push (Fmt.str "[%s:dom%i]%s: %i -> %i\n" (timestamp ts) d s (i :> int) (j :> int)) q in
    match Runtime_events.User.tag c with
    | Ctf.Try_read -> event "try-read"
    | Ctf.Read -> event "read"
    | Ctf.Signal -> event "signal"
    | Ctf.Value -> event "value"
    | _ -> ()
  in
  let id_callback d ts c i = match Runtime_events.User.tag c with
   | Ctf.Resolved -> Queue.push (Fmt.str "[%s:dom%i]resolved: %i\n" (timestamp ts) d (i :> int)) q 
   | _ -> ()
  in
  let event_callback d ts c () = match Runtime_events.User.tag c with
   | Ctf.Suspend -> Queue.push (Fmt.str "[%s:dom%i]suspend\n" (timestamp ts) d) q  
   | _ -> ()
  in
  add_callback Ctf.created_type id_event_callback evs |>
  add_callback Ctf.labelled_type id_string_callback |>
  add_callback Ctf.two_ids_type id_id_callback |>
  add_callback Runtime_events.Type.event event_callback |>
  add_callback Runtime_events.Type.counter id_callback

let task_events q =
  let module Queue = Eio_utils.Lf_queue in
  let evs = Runtime_events.Callbacks.create () in
  let id_event_callback d ts c ((i : Ctf.id), v) = 
    match Runtime_events.User.tag c, v with
    | Ctf.Created, Ctf.Task -> Queue.push q (`Created, ((i :> int), d, ts))
    | _ -> ()
  in
  let id_callback d ts c i = match Runtime_events.User.tag c with
    | Ctf.Resolved -> Queue.push q (`Resolved, (i, d, ts))
    | _ -> ()
  in
  add_callback Ctf.created_type id_event_callback evs |>
  add_callback Runtime_events.Type.counter id_callback

module Console = struct
  open Nottui
  module W = Nottui_widgets

  (* Very inefficient! *)
  let tasks_map = Hashtbl.create 128

  let tasks = Lwd_table.make ()

  let root () =
    let render_column _ (id, dom, ts) = W.fmt "dom: %i, id: %i, ts: %Ld" dom id (Runtime_events.Timestamp.to_int64 ts) in
    let table =
      Lwd_table.map_reduce render_column Ui.pack_y tasks
    in
    table |> W.scroll_area

  let add_tasks (i, d, ts) =
    Hashtbl.add tasks_map i (d, ts);
    Lwd_table.clear tasks;
    Hashtbl.iter (fun i (d, ts) -> Lwd_table.append' tasks (i, d, ts)) tasks_map

  let remove_task i =
    Hashtbl.remove tasks_map i;
    Lwd_table.clear tasks;
    Hashtbl.iter (fun i (d, ts) -> Lwd_table.append' tasks (i, d, ts)) tasks_map
end

let write_events ~clock write poll handle =
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let print_events = print_events q in
  while not (poll ()) do
    let _ = Runtime_events.read_poll cursor print_events None in
    while not (Queue.is_empty q) do
      write (Queue.pop q)
    done;
    Eio.Time.sleep clock 0.1
  done

let ui handle =
  let module Queue = Eio_utils.Lf_queue in
  let q = Queue.create () in
  let cursor = Runtime_events.create_cursor (Some handle) in
  let print_events = task_events q in
  Nottui.Ui_loop.run ~tick:(fun () -> 
    let _ = Runtime_events.read_poll cursor print_events None in
    while not (Queue.is_empty q) do 
      match Queue.pop q with 
      | None -> () 
      | Some (`Created, v) -> Console.add_tasks v
      | Some (`Resolved, (v, _, _)) -> Console.remove_task v
    done) ~tick_period:0.2 (Console.root ())

