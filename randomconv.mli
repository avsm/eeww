(** Randomconv - random in various representations

    Given a generator for random byte vectors, this convenience library converts
    the output into common number representations: int8, int16, int32, int64,
    int, and float.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}
*)

type int8 = int

(** [int8 g] is [r], a random [int8] using the generator [g]. *)
val int8 : (int -> Cstruct.t) -> int8

type int16 = int

(** [int16 g] is [r], a random [int16] using the generator [g]. *)
val int16 : (int -> Cstruct.t) -> int16

(** [int32 g] is [r], a random [int32] using the generator [g]. *)
val int32 : (int -> Cstruct.t) -> int32

(** [int64 g] is [r], a random [int64] using the generator [g]. *)
val int64 : (int -> Cstruct.t) -> int64

(** [int ~bound g] is [r], a random [int] between inclusive 0 and exclusive
    [bound] (defaults to [max_int]), using the generator [g].  [int] raises
    [Invalid_argument] if the supplied [bound] is smaller or equal to 0.  *)
val int : ?bound:int -> (int -> Cstruct.t) -> int

(** [float ~bound g] is [r], a random [float] between inclusive 0.0 and
    exclusive [bound] (defaults to [1.0]), using the generator [g].  [float]
    raises [Invalid_argument] if the supplied [bound] is smaller or equal to
    0. *)
val float : ?bound:float -> (int -> Cstruct.t) -> float
