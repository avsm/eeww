open! Import

type t = Caml.out_channel

val stdout : t
val stderr : t

type 'a with_create_args =
  ?binary:bool            (** defaults to [true] *)
  -> ?append:bool         (** defaults to [false] *)
  -> ?fail_if_exists:bool (** defaults to [false] *)
  -> ?perm:int
  -> 'a

val create : (string -> t) with_create_args
val with_file : (string -> f:(t -> 'a) -> 'a) with_create_args


(** [close t] flushes and closes [t], and may raise an exception.  [close] returns () and
    does not raise if [t] is already closed.  [close] raises an exception if the close()
    system call on the underlying file descriptor fails (i.e. returns -1), which would
    happen in the following cases:

    EBADF -- this would happen if someone else did close() system call on the underlying
    fd, which I would think a rare event.

    EINTR -- would happen if the system call was interrupted by a signal, which would be
    rare.  Also, I think we should probably just catch EINTR and re-attempt the close.
    Unfortunately, we can't do that in OCaml because the OCaml library marks the
    out_channel as closed even if the close syscall fails, so a subsequent call
    [close_out_channel] will be a no-op.  This should really be fixed in the OCaml library
    C code, having it restart the close() syscall on EINTR.  I put a couple CRs in
    [fixed_close_channel], our rework of OCaml's [caml_ml_close_channel],

    EIO -- I don't recall seeing this.  I think it's rare.

    See "man 2 close" for details.
*)
val close : t -> unit

val set_binary_mode : t -> bool -> unit

val flush : t -> unit

val output : t -> buf:string -> pos:int -> len:int -> unit
val output_string : t -> string -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_binary_int : t -> int -> unit
val output_value : t -> _ -> unit  (** OCaml's internal Marshal format *)

val newline : t -> unit

(** Outputs a list of lines, each terminated by a newline character *)
val output_lines : t -> string list -> unit

(** Formatted printing to an out channel.  This is the same as [Printf.sprintf] except
    that it outputs to [t] instead of returning a string.  Similarly, the function
    arguments corresponding to conversions specifications such as [%a] or [%t] takes [t]
    as argument and must print to it instead of returning a string. *)
val fprintf : t -> ('a, t, unit) format -> 'a

(** [printf fmt] is the same as [fprintf stdout fmt] *)
val printf : ('a, t, unit) format -> 'a

(** [printf fmt] is the same as [fprintf stderr fmt] *)
val eprintf : ('a, t, unit) format -> 'a

(** [kfprintf k t fmt] is the same as [fprintf t fmt], but instead of returning
    immediately, passes the out channel to [k] at the end of printing. *)
val kfprintf : (t -> 'a) -> t -> ('b, t, unit, 'a) format4 -> 'b

(** [print_endline str] outputs [str] to [stdout] followed by a newline then flushes
    [stdout] *)
val print_endline : string -> unit

(** [prerr_endline str] outputs [str] to [stderr] followed by a newline then flushes
    [stderr] *)
val prerr_endline : string -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64

(** The first argument of these is the file name to write to. *)
val write_lines : string -> string list -> unit
val write_all : string -> data:string -> unit


