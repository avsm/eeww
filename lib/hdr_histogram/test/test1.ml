open Hdr_histogram

let print_percentiles h =
  let percentiles = [| 50.0; 75.0; 90.0; 99.0; 99.9; 99.99; 99.999; 100.0 |] in
  Fun.flip Array.iter percentiles (fun p ->
    Printf.printf "%f \t %d\n" p (value_at_percentile h p))

let main () =
  let h = init ~lowest_discernible_value:1 ~highest_trackable_value:100_000_000
               ~significant_figures:3
  in
  for i=1000 to 100_000 do
    for _j =1 to 1000 do
      ignore @@ record_value h i
    done;
  done;
  for i=100_001 to 110_000 do
    for _j = 1 to 100 do
      ignore @@ record_value h i
    done;
  done;
  print_percentiles h;
  close h

let _ = main ()
