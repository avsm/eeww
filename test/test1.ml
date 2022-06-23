open Hdr_histogram

let main () =
  let h = hdr_init ~lowest_discernible_value:1
                  ~highest_trackable_value:100_000_000
                  ~significant_figures:3
  in
  for i=1000 to 100_000 do
    for _j =1 to 1000 do
      ignore @@ hdr_record_value h i
    done;
  done;
  for i=100_001 to 110_000 do
    for _j = 1 to 100 do
      ignore @@ hdr_record_value h i
    done;
  done;
  hdr_percentiles_print h ~ticks_per_half_distance:5 ~value_scale:1000.;
  hdr_close h

let _ = main ()
