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

(** Utility functions over flows *)

module Fun : (module type of Fflow)
(** Flows from functions *)

module CopyStats : sig
  type t = {
    read_bytes: int64;
    read_ops: int64;
    write_bytes: int64;
    write_ops: int64;
    duration: float;
  }
  (** I/O statistics from a copy operation *)

  val to_string: t -> string
end


val copy:
     (module V1.CLOCK)
  -> (module V1_LWT.FLOW with type flow = 'a) -> 'a
  -> (module V1_LWT.FLOW with type flow = 'b) -> 'b
  -> unit -> [ `Ok of CopyStats.t | `Error of [ `Msg of string ] ] Lwt.t
(** [copy (module Clock) (module Source) source (module Destination)
    destination] copies data from [source] to [destination] using the
    clock to compute a transfer rate. On successful completion, some statistics
    are returned. On failure we return a printable error. *)

val proxy:
     (module V1.CLOCK)
  -> (module Mirage_flow_s.SHUTDOWNABLE with type flow = 'a) -> 'a
  -> (module Mirage_flow_s.SHUTDOWNABLE with type flow = 'b) -> 'b
  -> unit -> [ `Ok of (CopyStats.t * CopyStats.t) | `Error of [ `Msg of string ] ] Lwt.t
(** [proxy (module Clock) (module A) a (module B) b ()] proxies data between
    [a] and [b] until both sides close. If either direction encounters an error
    then so will [proxy]. If both directions succeed, then return I/O statistics. *)
