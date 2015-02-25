(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let (>>=) = Lwt.bind

type 'a io = 'a Lwt.t
type buffer = Cstruct.t

type flow = {
  close: unit -> unit Lwt.t;
  perform_io: Cstruct.t -> int -> int -> int Lwt.t;
  mutable buf: Cstruct.t;
  mutable ic_closed: bool;
  mutable oc_closed: bool;
}

let default_buffer_size = 4096

let make ?(close=fun () -> Lwt.return_unit) perform_io =
  let buf = Cstruct.create default_buffer_size in
  { close; perform_io; buf; ic_closed = false; oc_closed = false; }

let read_string ?(chunk_size=max_int) str =
  let str_off = ref 0 in
  let str_len = String.length str in
  let perform_io buf off len =
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      let len = min len chunk_size in
      Cstruct.blit_from_string str !str_off buf off len;
      str_off := !str_off + len;
      Lwt.return len
    )
  in
  let ic = make perform_io in
  ic.oc_closed <- true;
  ic

let write_string ?(chunk_size=max_int) str =
  let str_off = ref 0 in
  let str_len = String.length str in
  let perform_io buf off len =
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      let len = min len chunk_size in
      Cstruct.blit_to_string buf off str !str_off len;
      str_off := !str_off + len;
      Lwt.return len
    )
  in
  let oc = make perform_io in
  oc.ic_closed <- true;
  oc

let read_cstruct ?(chunk_size=max_int) str =
  let str_off = ref 0 in
  let str_len = Cstruct.len str in
  let perform_io buf off len =
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      let len = min len chunk_size in
      Cstruct.blit str !str_off buf off len;
      str_off := !str_off + len;
      Lwt.return len
    )
  in
  let ic = make perform_io in
  ic.oc_closed <- true;
  ic

let write_cstruct ?(chunk_size=max_int) str =
  let str_off = ref 0 in
  let str_len = Cstruct.len str in
  let perform_io buf off len =
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      let len = min len chunk_size in
      Cstruct.blit buf off str !str_off len;
      str_off := !str_off + len;
      Lwt.return len
    )
  in
  let ic = make perform_io in
  ic.oc_closed <- true;
  ic

type error = [`None]

let pp_error ppf (e:error) = match e with
  | `None -> Format.pp_print_string ppf "<none>"

let refill ch =
  if Cstruct.len ch.buf = 0 then (
    let buf = Cstruct.create default_buffer_size in
    ch.buf <- buf
  )

let err_eof = Lwt.return `Eof

let read ch =
  if ch.ic_closed then err_eof
  else (
    refill ch;
    ch.perform_io ch.buf 0 default_buffer_size >>= fun n ->
    if n = 0 then (
      ch.ic_closed <- true;
      Lwt.return `Eof;
    ) else (
      let ret = Cstruct.sub ch.buf 0 n in
      let buf = Cstruct.shift ch.buf n in
      ch.buf <- buf;
      Lwt.return (`Ok ret)
    )
  )

let write ch buf =
  if ch.oc_closed then err_eof
  else (
    let len = Cstruct.len buf in
    let rec aux off =
      if off = len then Lwt.return (`Ok ())
      else (
        ch.perform_io buf off (len - off) >>= fun n ->
        if n = 0 then (
          ch.oc_closed <- true;
          Lwt.return `Eof
        ) else aux (off+n)
      )
    in
    aux 0
  )

let writev ch bufs =
  if ch.oc_closed then err_eof
  else
    let rec aux = function
      | []   -> Lwt.return (`Ok ())
      | h::t ->
        write ch h >>= function
        | `Error e -> Lwt.return (`Error e)
        | `Eof     -> Lwt.return `Eof
        | `Ok ()   -> aux t
    in
    aux bufs

let close ch =
  ch.ic_closed <- true;
  ch.oc_closed <- true;
  ch.close ()
