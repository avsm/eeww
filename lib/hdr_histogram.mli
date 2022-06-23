type t

val hdr_init : lowest_discernible_value:int
            -> highest_trackable_value:int
            -> significant_figures:int
            -> t

val hdr_record_value : t -> int -> bool
val hdr_close : t -> unit
val hdr_percentiles_print : t
                         -> ticks_per_half_distance:int
                         -> value_scale:float
                         -> unit
