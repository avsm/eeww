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

(** Function-based flows. *)

type error
(** Type for errors. *)

val pp_error: Format.formatter -> error -> unit
(** Pretty-print errors. *)

include V1_LWT.FLOW with type error := error

type refill = Cstruct.t -> int -> int -> int Lwt.t
(** The type for refill functions. *)

val make:
  ?close:(unit -> unit Lwt.t) -> ?input:refill -> ?output:refill -> unit -> flow
(** [make ~close ~input ~output ()] is a flow using [input] to refill
    its internal input buffer when needed and [output] to refill its
    external output buffer. It is using [close] to eventually clean-up
    other resources on close. *)

(** {1 String flows} *)

val input_string: string -> refill
(** [input_string buf] is the refill function reading its inputs from
    the string [buf]. *)

val output_string: string -> refill
(** [output_string buf] is the refill function writing its outputs in
    the buffer [buf]. *)

val string: ?input:string -> ?output:string -> unit -> flow
(** The flow built using {!input_string} and {!output_string}. *)

val input_strings: string list -> refill
(** [input_strings bufs] is the refill function reading its inputs from
    the list of buffers [bufs]. Empty strings are ignored. *)

val output_strings: string list -> refill
(** [output_strings buf] is the refill function writing its outputs in
    the list of buffers [buf]. Empty strings are ignored. *)

val strings: ?input:string list -> ?output:string list -> unit -> flow
(** The flow built using {!input_strings} and {!output_strings}. *)

(** {1 Cstruct buffers flows} *)

val input_cstruct: Cstruct.t -> refill
(** Same as {!input_string} but for {!Cstruct.t} buffers. *)

val output_cstruct: Cstruct.t -> refill
(** Same as {!output_string} buf for {!Cstruct.t} buffers. *)

val cstruct: ?input:Cstruct.t -> ?output:Cstruct.t -> unit -> flow
(** Same as {!string} but for {!Cstruct.t} buffers. *)

val input_cstructs: Cstruct.t list -> refill
(** Same as {!input_strings} but for {!Cstruct.t} buffers. *)

val output_cstructs: Cstruct.t list -> refill
(** Same as {!output_strings} but for {!Cstruct.t} buffers. *)

val cstructs: ?input:Cstruct.t list -> ?output:Cstruct.t list -> unit -> flow
(** Same as {!strings} but for {!Cstruct.t} buffers. *)
