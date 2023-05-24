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

module Exn = struct
  exception Retry_read
  exception Retry_write
  exception Too_many_polls

  exception
    Ssl_exception of
      { ssl_error : Ssl.ssl_error
      ; message : string
      }
end

module Unix_fd = struct
  let get_exn (fd : Eio.Net.stream_socket) =
    let fd = Option.get (Eio_unix.Resource.fd_opt fd) in
    Eio_unix.Fd.use_exn "Unix_fd.get_exn" fd (fun fd -> fd)
end

module Context = struct
  type state =
    | Uninitialized
    | Connected
    | Shutdown of exn option

  type t =
    { flow : Eio.Flow.two_way
    ; ctx : Ssl.context
    ; ssl_socket : Ssl.socket
    ; mutable state : state
    }

  let create ~ctx flow =
    let flow = (flow :> Eio.Flow.two_way) in
    let ssl_socket = Ssl.embed_socket (Unix_fd.get_exn flow) ctx in
    { flow; ctx; ssl_socket; state = Uninitialized }

  let get_fd t = t.flow

  let get_unix_fd t =
    match t.state with
    | Uninitialized | Shutdown _ -> Unix_fd.get_exn t.flow
    | Connected -> Ssl.file_descr_of_socket t.ssl_socket

  let ssl_socket t = t.ssl_socket
end

module Raw = struct
  open Context

  let wrap_call ~f t =
    try f () with
    | ( Ssl.Connection_error err
      | Ssl.Accept_error err
      | Ssl.Read_error err
      | Ssl.Write_error err ) as e ->
      (match err with
      | Ssl.Error_want_read -> raise_notrace Exn.Retry_read
      | Ssl.Error_want_write -> raise_notrace Exn.Retry_write
      | Ssl.Error_syscall | Ssl.Error_ssl ->
        (* From https://www.openssl.org/docs/man1.1.1/man3/SSL_get_error.html:
         * If this error occurs then no further I/O operations should be
         * performed on the connection and SSL_shutdown() must not be called.
         *)
        let exn =
          Exn.Ssl_exception
            { ssl_error = err; message = Ssl.get_error_string () }
        in
        t.state <- Shutdown (Some exn);
        raise exn
      | _ -> raise e)

  let repeat_call ~f t =
    let rec inner polls_remaining flow f =
      if polls_remaining <= 0
      then raise Exn.Too_many_polls
      else
        try wrap_call ~f t with
        | Exn.Retry_read ->
          Eio_unix.await_readable (Unix_fd.get_exn flow);
          inner (polls_remaining - 1) flow f
        | Exn.Retry_write ->
          Eio_unix.await_writable (Unix_fd.get_exn flow);
          inner (polls_remaining - 1) flow f
    in
    inner 64 t.flow f

  let accept t =
    Unix.set_nonblock (Unix_fd.get_exn t.flow);
    repeat_call t ~f:(fun () -> Ssl.accept t.ssl_socket)

  let connect t =
    Unix.set_nonblock (Unix_fd.get_exn t.flow);
    repeat_call t ~f:(fun () -> Ssl.connect t.ssl_socket)

  let read t buf =
    let { flow; state; ssl_socket; _ } = t in
    match state with
    | Shutdown (Some _) -> raise End_of_file
    | Uninitialized | Shutdown None -> Eio.Flow.single_read flow buf
    | Connected ->
      if buf.len = 0
      then 0
      else
        repeat_call t ~f:(fun () ->
            match
              Ssl.read_into_bigarray ssl_socket buf.buffer buf.off buf.len
            with
            | n -> n
            | exception Ssl.Read_error Ssl.Error_zero_return ->
              (* From https://www.openssl.org/docs/man1.1.1/man3/SSL_get_error.html:
               *
               *   SSL_ERROR_ZERO_RETURN
               *     The TLS/SSL peer has closed the connection for writing by
               *     sending the close_notify alert. No more data can be read
               *)
              raise End_of_file)

  let writev t bufs =
    let { ssl_socket; _ } = t in
    let rec do_write buf ~off ~len =
      match
        repeat_call t ~f:(fun () ->
            match Ssl.write_bigarray ssl_socket buf off len with
            | n -> n
            | exception Ssl.Write_error Ssl.Error_zero_return ->
              raise End_of_file)
      with
      | n when n < len -> n + do_write buf ~off:(off + n) ~len:(len - n)
      | n -> n
    in

    if Cstruct.lenv bufs = 0
    then 0
    else
      List.fold_left
        (fun acc (buf : Cstruct.t) ->
          acc + do_write buf.buffer ~off:buf.off ~len:buf.len)
        0
        bufs

  let copy t src =
    let do_rsb rsb =
      try
        while true do
          rsb (writev t)
        done
      with
      | End_of_file -> ()
    in
    match Eio.Flow.read_methods src with
    | Eio.Flow.Read_source_buffer rsb :: _
    | _ :: Eio.Flow.Read_source_buffer rsb :: _ ->
      do_rsb rsb
    | xs ->
      (match
         List.find_map
           (function Eio.Flow.Read_source_buffer rsb -> Some rsb | _ -> None)
           xs
       with
      | Some rsb -> do_rsb rsb
      | None ->
        (try
           while true do
             let buf = Cstruct.create 4096 in
             let got = Eio.Flow.single_read src buf in
             ignore (writev t [ Cstruct.sub buf 0 got ] : int)
           done
         with
        | End_of_file -> ()))

  let shutdown t cmd =
    match cmd with
    | `Receive -> ()
    | `Send | `All ->
      (match t.state with
      | Uninitialized | Shutdown _ -> ()
      | Connected ->
        if Ssl.close_notify t.ssl_socket then t.state <- Shutdown None)
end

type t = < Eio.Flow.two_way ; t : Context.t >

let of_t t =
  object
    inherit Eio.Flow.two_way
    method read_into = Raw.read t
    method copy = Raw.copy t
    method shutdown = Raw.shutdown t
    method t = t
  end

let accept (t : Context.t) =
  assert (t.state = Uninitialized);
  Raw.accept t;
  of_t { t with state = Connected }

let connect (t : Context.t) =
  assert (t.state = Uninitialized);
  Raw.connect t;
  of_t { t with state = Connected }

let ssl_socket t = Context.ssl_socket t#t
