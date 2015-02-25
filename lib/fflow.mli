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

(** Function based flows. *)

type error
(** Type for errors. *)

val pp_error: Format.formatter -> error -> unit
(** Pretty-print errors. *)

include V1_LWT.FLOW with type error := error

val make: ?close:(unit -> unit Lwt.t) -> (Cstruct.t -> int -> int -> int Lwt.t) -> flow
(** [make ~close refill] is an abstract flow using [refill] to refill
    internal buffer when needed, and using [close] to clean-up other
    resources on close. *)

(** {1 Readers} *)

val read_string: ?chunk_size:int -> string -> flow
(** Build a flow from reading string, using the given chunk size. The
    output flow is closed. *)

val read_cstruct: ?chunk_size:int -> Cstruct.t -> flow
(** Build a flow from reading a cstruct buffer. using the given chunk
    size. The corresponding output flow is closed. *)

(** {1 Writers} *)

val write_string: ?chunk_size:int -> string -> flow
(** Build a flow from reading string, using the given chunk size. The
    output flow is closed. *)

val write_cstruct: ?chunk_size:int -> Cstruct.t -> flow
(** Build a flow from reading a cstruct buffer. using the given chunk
    size. The corresponding output flow is closed. *)
