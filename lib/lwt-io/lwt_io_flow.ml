(*
 * Copyright (c) 2014 Citrix Systems Inc
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

module Make (F: V1_LWT.FLOW) = struct

  let reader t =
    let frag = ref (Cstruct.create 0) in
    let rec aux buf ofs len =
      if len = 0
      then Lwt.return 0
      else
        let available = Cstruct.len !frag in
        if available = 0 then begin
          F.read t >>= function
          | `Ok b ->
            frag := b;
            aux buf ofs len
          | `Eof -> Lwt.return 0
          | `Error e -> Lwt.fail (Failure "Lwt_io_flow.reader")
          (* FIXME: F.pp_error *)
        end else begin
          let n = min available len in
          Cstruct.blit !frag 0 (Cstruct.of_bigarray buf) ofs n;
          frag := Cstruct.shift !frag n;
          Lwt.return n
        end in
    aux

  let writer t buf ofs len =
    let b = Cstruct.sub (Cstruct.of_bigarray buf) ofs len in
    F.write t b >>= function
    | `Ok ()   -> Lwt.return len
    | `Eof     -> Lwt.return 0
    | `Error e -> (* FIXME: pp_error *) Lwt.fail (Failure "Lwt_io_flow.writer")

  let ic ?(buffer_size=1024) ?(close=true) t =
    let close () = if close then F.close t else Lwt.return_unit in
    let buffer = Lwt_bytes.create buffer_size in
    Lwt_io.make ~buffer ~mode:Lwt_io.input ~close (reader t)

  let oc ?(buffer_size=1024) ?(close=false) t =
    let close () = if close then F.close t else Lwt.return_unit in
    let buffer = Lwt_bytes.create buffer_size in
    Lwt_io.make ~buffer ~mode:Lwt_io.output ~close (writer t)

end
