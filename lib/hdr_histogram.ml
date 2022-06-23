open Ctypes

module Types = Types_generated

type t = Types.hdr_histogram structure ptr

let hdr_init ~lowest_discernible_value ~highest_trackable_value
             ~significant_figures =
  let h = allocate (ptr Types.hdr_histogram)
                   (from_voidp (Types.hdr_histogram) null)
  in
  let res = C.Function.hdr_init (Int64.of_int lowest_discernible_value)
                                (Int64.of_int highest_trackable_value)
                                significant_figures h
  in
  assert (res = 0);
  let h' : t = !@ h in
  h'

let hdr_record_value h v = C.Function.hdr_record_value h (Int64.of_int v)

let hdr_close h = C.Function.hdr_close h

let hdr_percentiles_print h ~ticks_per_half_distance ~value_scale =
  let r = C.Function.hdr_percentiles_print h (Int64.of_int 1241085104)
            (Int32.of_int ticks_per_half_distance) value_scale (Int64.of_int 0)
  in
  assert (r = 0)
