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

let pp_str fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); Buffer.contents b in
  Format.kfprintf k ppf fmt

let pp_buf ppf buf = Format.pp_print_string ppf (Cstruct.to_string buf)

let cmp_cstruct b1 b2 = Cstruct.to_string b1 = Cstruct.to_string b2

let rec cmp_list fn l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _ , [] -> false
  | h1::t1, h2::t2 -> fn h1 h1 && cmp_list fn t1 t2

let printer_list fn l = String.concat ", " (List.map fn l)

let fail = OUnit.assert_string

let check_buffer msg b1 b2 =
  OUnit.assert_equal ~cmp:cmp_cstruct ~printer:Cstruct.to_string ~msg b1 b2

let check_buffers msg b1s b2s =
  OUnit.assert_equal
    ~cmp:(cmp_list cmp_cstruct) ~printer:(printer_list Cstruct.to_string)
    ~msg b1s b2s

let check_ok_buffer msg buf = function
  | `Ok b    -> check_buffer msg buf b
  | `Error e -> fail (pp_str "%s: error=%a" msg Fflow.pp_error e)
  | `Eof     -> fail (pp_str "%s: eof" msg)

let check_ok_unit msg = function
  | `Ok ()   -> ()
  | `Error e -> fail (pp_str "%s: error=%a" msg Fflow.pp_error e)
  | `Eof     -> fail (pp_str "%s: eof" msg)

let check_eof msg = function
  | `Ok _    -> fail (Printf.sprintf "%s: ok" msg)
  | `Error e -> fail (pp_str "%s: error=%a" msg Fflow.pp_error e)
  | `Eof     -> ()

let cs str = Cstruct.of_string str
let css = List.map cs
let filter x =
  let zero = Cstruct.of_string "" in
  List.filter ((<>) zero) x

let input_string () =
  let input = "xxxxxxxxxx" in
  let ic = Fflow.string ~input () in
  Fflow.read ic >>= fun x1 ->
  Fflow.read ic >>= fun x2 ->
  Fflow.write ic (cs "hihi") >>= fun r ->
  check_ok_buffer "read 1" (cs input) x1;
  check_eof       "read 2" x2;
  check_eof       "write"  r;
  Lwt.return_unit

let output_string () =
  let output = "xxxxxxxxxx" in
  let oc = Fflow.string ~output () in
  Fflow.write oc (cs  "hell") >>= fun x1 ->
  Fflow.write oc (cs   "o! ") >>= fun x2 ->
  Fflow.write oc (cs "world") >>= fun x3 ->
  Fflow.read oc >>= fun r ->
  check_buffer  "result" (cs output) (cs "hello! wor");
  check_ok_unit "write 1" x1;
  check_ok_unit "write 2" x2;
  check_eof     "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

let input_strings () =
  let input = [ ""; "123"; "45"; "6789"; "0" ] in
  let ic = Fflow.strings ~input () in
  Fflow.read ic >>= fun x1 ->
  Fflow.read ic >>= fun x2 ->
  Fflow.read ic >>= fun x3 ->
  Fflow.read ic >>= fun x4 ->
  Fflow.read ic >>= fun y ->
  Fflow.read ic >>= fun z ->
  Fflow.write ic (cs "hihi") >>= fun w ->
  check_ok_buffer "read 1" (cs  "123") x1;
  check_ok_buffer "read 2" (cs   "45") x2;
  check_ok_buffer "read 3" (cs "6789") x3;
  check_ok_buffer "read 4" (cs    "0") x4;
  check_eof       "read 5" y;
  check_eof       "read 6" z;
  check_eof       "write"  w;
  Lwt.return_unit

let output_strings () =
  let output = ["xxx"; ""; "xx"; "xxx"; ] in
  let oc = Fflow.strings ~output () in
  Fflow.write oc (cs  "hell") >>= fun x1 ->
  Fflow.write oc (cs   "o! ") >>= fun x2 ->
  Fflow.write oc (cs "world") >>= fun x3 ->
  Fflow.read oc >>= fun r ->
  check_buffers "result" (filter (css output)) (css ["hel"; "lo"; "! w"]);
  check_ok_unit "write 1" x1;
  check_ok_unit "write 2" x2;
  check_eof     "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

let input_cstruct () =
  let input = Cstruct.of_string "xxxxxxxxxx" in
  let ic = Fflow.cstruct ~input () in
  Fflow.read ic >>= fun x1 ->
  Fflow.read ic >>= fun x2 ->
  Fflow.write ic (cs "hihi") >>= fun r ->
  check_ok_buffer "read 1" input x1;
  check_eof       "read 2" x2;
  check_eof       "write"  r;
  Lwt.return_unit

let output_cstruct () =
  let output = Cstruct.of_string "xxxxxxxxxx" in
  let oc = Fflow.cstruct ~output () in
  Fflow.write oc (cs  "hell") >>= fun x1 ->
  Fflow.write oc (cs   "o! ") >>= fun x2 ->
  Fflow.write oc (cs "world") >>= fun x3 ->
  Fflow.read oc >>= fun r ->
  check_buffer  "result" output (cs "hello! wor");
  check_ok_unit "write 1" x1;
  check_ok_unit "write 2" x2;
  check_eof     "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

let input_cstructs () =
  let inputs = List.map cs [ "123"; "45"; ""; "6789"; "0" ] in
  let ic = Fflow.cstructs ~input:inputs () in
  Fflow.read ic >>= fun x1 ->
  Fflow.read ic >>= fun x2 ->
  Fflow.read ic >>= fun x3 ->
  Fflow.read ic >>= fun x4 ->
  Fflow.read ic >>= fun y ->
  Fflow.read ic >>= fun z ->
  Fflow.write ic (cs "hihi") >>= fun w ->
  check_ok_buffer "read 1" (cs  "123") x1;
  check_ok_buffer "read 2" (cs   "45") x2;
  check_ok_buffer "read 3" (cs "6789") x3;
  check_ok_buffer "read 4" (cs    "0") x4;
  check_eof       "read 5 "y;
  check_eof       "read 6" z;
  check_eof       "read 7" w;
  Lwt.return_unit

let output_cstructs () =
  let output = List.map cs [ ""; "xxx"; "xx"; "xxx" ] in
  let oc = Fflow.cstructs ~output () in
  Fflow.write oc (cs  "hell") >>= fun x1 ->
  Fflow.write oc (cs   "o! ") >>= fun x2 ->
  Fflow.write oc (cs "world") >>= fun x3 ->
  Fflow.read oc >>= fun r ->
  check_buffers "result" (filter output) (css ["hel"; "lo"; "! w"]);
  check_ok_unit "write 1" x1;
  check_ok_unit "write 2" x2;
  check_eof     "write 3" x3;
  check_eof     "read"    r;
  Lwt.return_unit

module Lwt_io_flow = Lwt_io_flow.Make (Fflow)

let input_lwt_io () =
  let ic = Fflow.strings ~input:["1"; "234"; "56"; "78\n90"] () in
  let lic = Lwt_io_flow.ic ic in
  Lwt_io.read_line lic >>= fun l ->
  check_buffer "result" (cs "12345678") (cs l);
  Lwt.return_unit

let output_lwt_io () =
  let output = css ["xxxx";"xxxx"; "xxxxxx"] in
  let oc = Fflow.cstructs ~output () in
  let loc = Lwt_io_flow.oc oc in
  Lwt_io.write_line loc "Hello world!" >>= fun () ->
  check_buffers "result" (css ["Hello"; " wor"; "ld!\nxx"]) output;
  Lwt.return_unit

let run f () = Lwt_main.run (f ())

let string = [
  "input" , `Quick, run input_string;
  "output", `Quick, run output_string;
]

let strings = [
  "input" , `Quick, run input_strings;
  "output", `Quick, run output_strings;
]

let cstruct = [
  "input" , `Quick, run input_cstruct;
  "output", `Quick, run output_cstruct;
]

let cstructs = [
  "input" , `Quick, run input_cstructs;
  "output", `Quick, run output_cstructs;
]

let lwt_io = [
  "input" , `Quick, run input_lwt_io;
  "output", `Quick, run output_lwt_io;
]
let () =
  Alcotest.run "mirage-flow" [
    "string"  , string;
    "strings" , strings;
    "cstruct" , cstruct;
    "cstructs", cstructs;
    "lwt-io"  , lwt_io;
  ]
