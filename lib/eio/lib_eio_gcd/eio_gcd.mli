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
 clock : Eio.Time.clock;
 fs : Eio.Fs.dir Eio.Path.t;
 cwd : Eio.Fs.dir Eio.Path.t;
 debug : Eio.Debug.t;
 domain_mgr : Eio.Domain_manager.t;
 mono_clock : Eio.Time.Mono.t;
 secure_random : Eio.Flow.source;
>

val run : (stdenv -> 'a) -> 'a
 (** The main loop *)
