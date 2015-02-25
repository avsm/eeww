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

open Printf
let (>>=) = Lwt.bind

let str = "1234567890"

let pp_str fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); Buffer.contents b in
  Format.kfprintf k ppf fmt

let pp_buf ppf buf = Format.pp_print_string ppf (Cstruct.to_string buf)

let cmp_cstruct b1 b2 = Cstruct.to_string b1 = Cstruct.to_string b2

let fail = OUnit.assert_string

let check_buffer buf = function
  | `Ok b    ->
    OUnit.assert_equal ~cmp:cmp_cstruct ~printer:Cstruct.to_string ~msg:"buffer"
      b buf
  | `Error e -> fail (pp_str "buffer: error=%a" Fflow.pp_error e)
  | `Eof     -> fail "buffer: eof"

let check_ok = function
  | `Ok () -> ()
  | `Error e -> fail (pp_str "ok: error=%a" Fflow.pp_error e)
  | `Eof     -> fail "ok: eof"

let check_eof = function
  | `Ok _    -> fail (Printf.sprintf "eof: ok")
  | `Error e -> fail (pp_str "eof: error=%a" Fflow.pp_error e)
  | `Eof     -> OUnit.assert_bool "eof" true

let cs str = Cstruct.of_string str

let input () =
  let ic = Fflow.read_string ~chunk_size:3 str in
  Fflow.read ic >>= fun x1 ->
  Fflow.read ic >>= fun x2 ->
  Fflow.read ic >>= fun x3 ->
  Fflow.read ic >>= fun x4 ->
  Fflow.read ic >>= fun y ->
  Fflow.read ic >>= fun z ->
  check_buffer (cs "123") x1;
  check_buffer (cs "456") x2;
  check_buffer (cs "789") x3;
  check_buffer (cs   "0") x4;
  check_eof y;
  check_eof z;
  Fflow.write ic (cs "hihi") >>= fun w ->
  check_eof w;
  Lwt.return_unit

let output () =
  let oc = Fflow.write_string ~chunk_size:3 str in
  Fflow.write oc (cs  "hell") >>= fun x1 ->
  Fflow.write oc (cs   "o! ") >>= fun x2 ->
  Fflow.write oc (cs "world") >>= fun x3 ->
  check_ok x1;
  check_ok x2;
  check_eof x3;
  Fflow.read oc >>= fun r ->
  check_eof r;
  Lwt.return_unit

let run f () = Lwt_main.run (f ())

let simple = [
  "input" , `Quick, run input;
  "output", `Quick, run output;
]

let () =
  Alcotest.run "mirage-flow" [
    "simple", simple
  ]
