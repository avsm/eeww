module IntMap = Map.Make(Int)

(* Should be LF queue! *)
type 'a handle = ((unit, unit) Effect.Deep.handler * (('a, exn) result Promise.u * (unit -> 'a)) Queue.t) Hmap.key

let _registered_handlers : Hmap.t ref = ref Hmap.empty
let registered_handlers () = _registered_handlers

let register_handler handler =
  let handlers_ref = registered_handlers () in
  let handlers = !handlers_ref in
  let uid = Hmap.Key.create () in
  let new_handlers = Hmap.add uid (handler, Queue.create ()) handlers in
  handlers_ref := new_handlers;
  uid

let lookup_handler_exn i = Hmap.get i !(registered_handlers ())

class virtual t = object
  method virtual run : 'a. ?loc:string -> (cancelled:exn Promise.t -> 'a) -> 'a
  method virtual run_raw : 'a. (unit -> 'a) -> 'a
  method virtual submit : 'a. ?loc:string -> 'a handle -> (unit -> 'a) -> 'a
end

let run_raw (t : #t) = t#run_raw

let submit ~loc (t : #t) = t#submit ~loc

let run ~loc (t : #t) fn =
  t#run ~loc @@ fun ~cancelled ->
  (* If the spawning fiber is cancelled, [cancelled] gets set to the exception. *)
  try
    Fiber.first ~loc
      (fun () ->
         match Promise.await cancelled with
         | Cancel.Cancelled ex -> raise ex    (* To avoid [Cancelled (Cancelled ex))] *)
         | ex -> raise ex (* Shouldn't happen *)
      )
      fn
  with ex ->
    match Promise.peek cancelled with
    | Some (Cancel.Cancelled ex2 as cex) when ex == ex2 ->
      (* We unwrapped the exception above to avoid [fn] seeing a double cancelled exception.
         But this means that the top-level reported the original exception,
         which isn't what we want. *)
      raise cex
    | _ -> raise ex

let[@inline always] run (t : #t) fn =
  let loc = Ctf.get_caller () in
  run ~loc t fn

let[@inline always] submit (t : #t) fn =
  let loc = Ctf.get_caller () in
  submit ~loc t fn