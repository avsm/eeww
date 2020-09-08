(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

(* Miscellaneous net-helpers used by Cohttp. Ideally, these will disappear
 * into some connection-management framework such as andrenth/release *)

open Lwt.Infix

module IO = Io

type resolvers = Conduit.resolvers

let () = Ssl.init ()

let default_ssl_context =
  Ssl.create_context Ssl.SSLv23 Ssl.Client_context

let empty =
  Conduit_lwt.empty
  |> Conduit_lwt.add Conduit_lwt.TCP.protocol
    (Conduit_lwt.TCP.resolve ~port:80)
  |> Conduit_lwt.add Conduit_lwt_ssl.TCP.protocol
    (Conduit_lwt_ssl.TCP.resolve ~port:443 ~context:default_ssl_context)
(* XXX(dinosaure) [cohttp-lwt-unix] provides a default resolve which is
 * able to start a simple TCP/IP connection or a TLS 1.3 connection to
 * handle [http] and [https] cases.
 *
 * The user is able to prioritize over these resolvers its own resolver
 * such as one with a specific [Ssl.context] (with TLS 1.3 support) if he wants. *)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let connect_uri ?host:(default= "localhost") ~resolvers uri =
  let domain_name = Domain_name.(host_exn (of_string_exn (Uri.host_with_default ~default uri))) in
  Conduit_lwt.resolve resolvers domain_name >>= function
  | Ok flow ->
    let ic, oc = Conduit_lwt.io_of_flow flow in
    Lwt.return (flow, ic, oc)
  | Error err ->
    failwith "%a" Conduit_lwt.pp_error err

let close c = Lwt.catch
  (fun () -> Lwt_io.close c)
  (fun e ->
    Logs.warn (fun f -> f "Closing channel failed: %s" (Printexc.to_string e));
    Lwt.return_unit
  )

let close_in ic = Lwt.ignore_result (close ic)

let close_out oc = Lwt.ignore_result (close oc)

let close ic oc = Lwt.ignore_result (close ic >>= fun () -> close oc)
