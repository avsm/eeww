(*
 * Mirage_flow_copyright (C) 2016 David Scott <dave.scott@docker.com>
 *
 * Permission to use, Mirage_flow_copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * Mirage_flow_copyright notice and this permission notice appear in all copies.
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
open Lwt

let proxy (module Clock: V1.CLOCK)
					(type a) (module A: Mirage_flow_s.SHUTDOWNABLE with type flow = a) (a: a)
					(type b) (module B: Mirage_flow_s.SHUTDOWNABLE with type flow = b) (b: b)
					() =
  let a2b =
    let t = Mirage_flow_copy.start (module Clock) (module A) a (module B) b () in
    Mirage_flow_copy.wait t
    >>= fun result ->
    A.shutdown_read a
    >>= fun () ->
    B.shutdown_write b
    >>= fun () ->
    let stats = Mirage_flow_copy.stats t in
    match result with
    | `Ok () -> return (`Ok stats)
    | `Error (`Msg m) -> return (`Error (`Msg m)) in
  let b2a =
    let t = Mirage_flow_copy.start (module Clock) (module B) b (module A) a () in
    Mirage_flow_copy.wait t
    >>= fun result ->
    B.shutdown_read b
    >>= fun () ->
    A.shutdown_write a
    >>= fun () ->
    let stats = Mirage_flow_copy.stats t in
    match result with
    | `Ok () -> return (`Ok stats)
    | `Error (`Msg m) -> return (`Error (`Msg m)) in
  a2b >>= fun a_stats ->
  b2a >>= fun b_stats ->
  match a_stats, b_stats with
  | `Ok a_stats, `Ok b_stats -> return (`Ok (a_stats, b_stats))
  | `Error (`Msg m1), `Error (`Msg m2) -> return (`Error (`Msg ("flow proxy a: " ^ m1 ^ "; flow proxy b: " ^ m1)))
  | `Error (`Msg m), _ -> return (`Error (`Msg ("flow proxy a: " ^ m)))
  | _, `Error (`Msg m) -> return (`Error (`Msg ("flow proxy b: " ^ m)))
