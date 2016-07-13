
type t = int64

let of_us m = Int64.(mul 1_000L (of_int m))

let of_ms m = Int64.(mul 1_000_000L (of_int m))

let of_sec s = Int64.(mul 1_000_000L (of_int s))

let of_min m = Int64.(mul 60_000_000L (of_int m))

let hour = 3600_000_000L

let of_hour h = Int64.(mul hour (of_int h))

let of_day d = Int64.(mul (mul 24L hour) (of_int d))

let of_year y = Int64.(mul (mul 8766L hour) (of_int y))

let of_f f =
  let s = int_of_float f in
  let rem = f -. (float_of_int s) in
  let ns = Int64.of_float (rem *. 1_000_000_000.) in
  Int64.(add (mul 1_000_000_000L (of_int s)) ns)


let to_us t = Int64.(to_int (div t 1_000L))

let to_ms t = Int64.(to_int (div t 1_000_000L))

let to_sec t = Int64.(to_int (div t 1_000_000_000L))

let to_min t = Int64.(to_int (div t 60_000_000_000L))

let to_hour t = Int64.(to_int (div t hour))

let to_day t = Int64.(to_int (div t (mul 24L hour)))

let to_year t = Int64.(to_int (div t (mul 8766L hour)))

let to_f t =
  let ns = Int64.to_float t in
  ns /. 1_000_000_000.


let pp ppf t =
  let hours = to_hour t in
  let left = Int64.rem t hour in
  let min = to_min left in
  let left = Int64.sub left (of_min min) in
  let sec = to_sec left in
  let left = Int64.sub left (of_sec sec) in
  let ms = to_ms left in
  let left = Int64.sub left (of_ms ms) in
  let us = to_us left in
  let ns = Int64.(to_int (sub left (of_us us))) in
  let p = ref false in
  if hours > 0 then
    (p := true ;
     Format.fprintf ppf "%d hours@ " hours) ;
  if (!p && (sec > 0 || ms > 0 || us > 0 || ns > 0)) || min > 0 then
    (p := true ;
     Format.fprintf ppf "%d minutes@ " min) ;
  if (!p && (ms > 0 || us > 0 || ns > 0)) || sec > 0 then
    (p := true ;
     Format.fprintf ppf "%d seconds@ " sec) ;
  if (!p && (us > 0 || ns > 0)) || ms > 0 then
    (p := true ;
     Format.fprintf ppf "%d milliseconds@ " ms) ;
  if (!p && ns > 0) || us > 0 then
    Format.fprintf ppf "%d microseconds@ " us ;
  if ns > 0 then
    Format.fprintf ppf "%d nanoseconds@ " ns
