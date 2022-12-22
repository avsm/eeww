val src : Logs.src

module File : sig
  type t
end

type has_fd = < fd : File.t >
type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

 type stdenv = <
 stdin  : source;
 stdout : sink;
 stderr : sink;
 net : Eio.Net.t;
 fs : Eio.Fs.dir Eio.Path.t;
 secure_random : Eio.Flow.source;
 clock : Eio.Time.clock;
>

val run : (stdenv -> unit) -> unit
 (** The main loop *)