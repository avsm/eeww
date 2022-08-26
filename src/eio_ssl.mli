(*
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2022 Antonio Nuno Monteiro
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

exception Too_many_polls

type socket
(** Wrapper for SSL sockets.

    It is either a plain socket, either a real SSL socket. *)

type uninitialized_socket
(** Wrapper for SSL sockets that have not yet performed the SSL handshake. *)

val ssl_socket : socket -> Ssl.socket option
(** Returns the underlying SSL socket used for this wrapper. If it is a plain
    socket it returns [None]. *)

val ssl_socket_of_uninitialized_socket : uninitialized_socket -> Ssl.socket
(** Returns the underlying SSL socket used for this wrapper. *)

val is_ssl : socket -> bool
(** Are we using an SSL socket? *)

val ssl_accept : Eio.Net.stream_socket -> Ssl.context -> socket
val ssl_connect : Eio.Net.stream_socket -> Ssl.context -> socket
val plain : Eio.Net.stream_socket -> socket
val embed_socket : Eio.Net.stream_socket -> Ssl.context -> socket

val embed_uninitialized_socket
  :  Eio.Net.stream_socket
  -> Ssl.context
  -> uninitialized_socket

val ssl_perform_handshake : uninitialized_socket -> socket
(** Initiate a SSL/TLS handshake on the specified socket (used by clients). *)

val ssl_accept_handshake : uninitialized_socket -> socket
(** Await a SSL/TLS handshake on the specified socket (used by servers). *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val read : socket -> off:int -> len:int -> bigstring -> int
val write_string : socket -> string -> int
val write : socket -> off:int -> len:int -> bigstring -> int
val shutdown : socket -> Eio.Flow.shutdown_command -> unit
val shutdown_and_close : socket -> unit
val ssl_shutdown : socket -> unit
val get_fd : socket -> Eio.Net.stream_socket
val get_unix_fd : socket -> Unix.file_descr
val getsockname : socket -> Unix.sockaddr
val getpeername : socket -> Unix.sockaddr
