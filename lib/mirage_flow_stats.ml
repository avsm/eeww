(*
 * Copyright (C) 2016 David Scott <dave.scott@docker.com>
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
 *
 *)

type t = {
  read_bytes: int64;
  read_ops: int64;
  write_bytes: int64;
  write_ops: int64;
  duration: float;
}

let kib = 1024L
let ( ** ) = Int64.mul
let mib = kib ** 1024L
let gib = mib ** 1024L
let tib = gib ** 1024L

let suffix = [
  kib, "KiB";
  mib, "MiB";
  gib, "GiB";
  tib, "TiB";
]

let add_suffix x =
  List.fold_left (fun acc (y, label) ->
      if Int64.div x y > 0L
      then Printf.sprintf "%.1f %s" Int64.((to_float x) /. (to_float y)) label
      else acc
    ) (Printf.sprintf "%Ld bytes" x) suffix

let to_string s =
  Printf.sprintf "%s bytes at %s/sec and %.1f IOPS/sec"
    (add_suffix s.read_bytes)
    (add_suffix Int64.(of_float ((to_float s.read_bytes) /. s.duration)))
    ((Int64.to_float s.read_ops) /. s.duration)
