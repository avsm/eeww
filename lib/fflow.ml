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

type refill = Cstruct.t -> int -> int -> int Lwt.t

let seq f1 f2 buf off len =
  f1 buf off len >>= function
  | 0 -> f2 buf off len
  | n -> Lwt.return n

let zero _buf _off _len = Lwt.return 0

let rec iter fn = function
  | []   -> zero
  | h::t -> seq (fn h) (iter fn t)

type flow = {
  close: unit -> unit Lwt.t;
  input: refill;
  output: refill;
  mutable buf: Cstruct.t;
  mutable ic_closed: bool;
  mutable oc_closed: bool;
}

let default_buffer_size = 4096

let make ?(close=fun () -> Lwt.return_unit) ?input ?output () =
  let buf = Cstruct.create default_buffer_size in
  let ic_closed = input = None in
  let oc_closed = output = None in
  let input = match input with None -> zero | Some x -> x in
  let output = match output with None -> zero | Some x -> x in
  { close; input; output; buf; ic_closed; oc_closed; }

let input_fn len blit str =
  let str_off = ref 0 in
  let str_len = len str in
  fun buf off len ->
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      blit str !str_off buf off len;
      str_off := !str_off + len;
      Lwt.return len
    )

let output_fn len blit str =
  let str_off = ref 0 in
  let str_len = len str in
  fun buf off len ->
    if !str_off >= str_len then Lwt.return 0
    else (
      let len = min (str_len - !str_off) len in
      blit buf off str !str_off len;
      str_off := !str_off + len;
      Lwt.return len
    )

let mk fn_i fn_o ?input ?output () =
  let input = match input with None -> None | Some x -> Some (fn_i x) in
  let output = match output with None -> None | Some x -> Some (fn_o x) in
  make ?input ?output ()

let input_string = input_fn String.length Cstruct.blit_from_string
let output_string = output_fn String.length Cstruct.blit_to_string
let string = mk input_string output_string

let input_cstruct = input_fn Cstruct.len Cstruct.blit
let output_cstruct = output_fn Cstruct.len Cstruct.blit
let cstruct = mk input_cstruct output_cstruct

let input_strings = iter input_string
let output_strings = iter output_string
let strings = mk input_strings output_strings

let input_cstructs = iter input_cstruct
let output_cstructs = iter output_cstruct
let cstructs = mk input_cstructs output_cstructs

type error = [`None]

let error_message = function
  | `None -> "<none>"

let pp_error ppf e = Format.pp_print_string ppf (error_message e)

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
    ch.input ch.buf 0 default_buffer_size >>= fun n ->
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
        ch.output buf off (len - off) >>= fun n ->
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
