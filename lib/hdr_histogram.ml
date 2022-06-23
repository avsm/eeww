open Ctypes

module Types = Types_generated

type t = Types.hdr_histogram structure ptr

let init ~lowest_discernible_value ~highest_trackable_value
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

let record_value h v = C.Function.hdr_record_value h (Int64.of_int v)

let close h = C.Function.hdr_close h

let value_at_percentile h p =
  Int64.to_int @@ C.Function.hdr_value_at_percentile h p
