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
open Lwt

type t = {
	_read_bytes: int64 ref;
	_read_ops: int64 ref;
	_write_bytes: int64 ref;
	_write_ops: int64 ref;
	_finish: float option ref;
	start: float;
	time: unit -> float;
	t: [ `Ok of unit | `Error of [ `Msg of string ] ] Lwt.t;
}

let stats t =
	let duration = match !(t._finish) with
	  | None -> t.time () -. t.start
		| Some x -> x -. t.start in {
	Mirage_flow_stats.read_bytes = !(t._read_bytes);
	read_ops = !(t._read_ops);
	write_bytes = !(t._write_bytes);
	write_ops = !(t._write_ops);
	duration;
}

let start (module Clock: V1.CLOCK)
					(type a) (module A: V1_LWT.FLOW with type flow = a) (a: a)
					(type b) (module B: V1_LWT.FLOW with type flow = b) (b: b)
					() =
	let _read_bytes = ref 0L in
	let _read_ops = ref 0L in
	let _write_bytes = ref 0L in
	let _write_ops = ref 0L in
	let _finish = ref None in
	let start = Clock.time () in
	let rec loop () =
		A.read a
		>>= function
		| `Eof ->
			_finish := Some (Clock.time ());
			Lwt.return (`Ok ())
		| `Ok buffer ->
			_read_ops := Int64.succ !_read_ops;
			_read_bytes := Int64.(add !_read_bytes (of_int @@ Cstruct.len buffer));
			begin B.write b buffer
			>>= function
			| `Ok () ->
				_write_ops := Int64.succ !_write_ops;
				_write_bytes := Int64.(add !_write_bytes (of_int @@ Cstruct.len buffer));
				loop ()
			| `Error m ->
				_finish := Some (Clock.time ());
				Lwt.return (`Error (`Msg (B.error_message m)))
			| `Eof ->
				_finish := Some (Clock.time ());
				Lwt.return (`Error (`Msg (Printf.sprintf "write failed with Eof")))
			end
		| `Error m ->
			_finish := Some (Clock.time ());
			Lwt.return (`Error (`Msg (A.error_message m))) in
	{
		_read_bytes;
		_read_ops;
		_write_bytes;
		_write_ops;
		_finish;
		start;
		time = Clock.time;
		t = loop ();
	}

let wait t = t.t

let copy (module Clock: V1.CLOCK)
				 (type a) (module A: V1_LWT.FLOW with type flow = a) (a: a)
				 (type b) (module B: V1_LWT.FLOW with type flow = b) (b: b)
				 () =
	let t = start (module Clock) (module A) a (module B) b () in
	wait t
	>>= function
	| `Ok () -> return (`Ok (stats t))
	| `Error (`Msg m) -> return (`Error (`Msg m))
