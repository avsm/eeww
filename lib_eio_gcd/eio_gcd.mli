val src : Logs.src

module File : sig
  type t
end

type has_fd = < fd : File.t >
type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

val get_fd : <has_fd; ..> -> File.t
val get_fd_opt : #Eio.Generic.t -> File.t option

type stdenv = <
 stdin  : source;
 stdout : sink;
 stderr : sink;
 net : Eio.Net.t;
 fs : Eio.Fs.dir Eio.Path.t;
 cwd : Eio.Fs.dir Eio.Path.t;
 secure_random : Eio.Flow.source;
 clock : Eio.Time.clock;
 mono_clock : Eio.Time.Mono.t;
 debug : Eio.Debug.t;
 domain_mgr : Eio.Domain_manager.t;
>

val run : (stdenv -> 'a) -> 'a
 (** The main loop *)