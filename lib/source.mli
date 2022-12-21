module Timer : sig
    type t
    (** A timer source *)

    val create : Queue.t -> t
    (** Create a new timer with events handled by the specified queue. *)

    val set_event_handler : t -> (unit -> unit) -> unit
    (** The event handler that is called once the timer runs and the delay
        has happened. *)

    val start : t -> int64 -> unit
    (** Start a timer *)

    val stop : t -> unit
    (** Stop a timer from being called *)
end