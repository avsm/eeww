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

module Exn : sig
  exception Too_many_polls

  exception
    Ssl_exception of
      { ssl_error : Ssl.ssl_error
      ; message : string
      }
end

module Context : sig
  type t

  val create : ctx:Ssl.context -> #Eio.Flow.two_way -> t
  val get_fd : t -> Eio.Net.stream_socket
  val get_unix_fd : t -> Unix.file_descr
  val ssl_socket : t -> Ssl.socket
end

type t = private < Eio.Flow.two_way ; .. >

val ssl_socket : t -> Ssl.socket

val accept : Context.t -> t
(** Accept a TLS Connection from a client *)

val connect : Context.t -> t
(** Connect to a server over TLS *)
