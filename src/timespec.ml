open Import

type t = {sec: int64; nsec: int64}

let sec t = t.sec

let nsec t = t.nsec

let nsec_per_sec = 1_000_000_000L

let rec normalize_timespec ~sec ~nsec =
  if nsec > nsec_per_sec then
    normalize_timespec ~sec:(Int64.succ sec) ~nsec:(Int64.sub nsec nsec_per_sec)
  else if nsec < Int64.zero then
    normalize_timespec ~sec:(Int64.pred sec) ~nsec:(Int64.add nsec nsec_per_sec)
  else (sec, nsec)

let make ~sec ~nsec =
  let sec, nsec = normalize_timespec ~sec ~nsec in
  {sec; nsec}

let to_cstruct {sec; nsec} =
  let open Ctypes in
  let timespec = make Bindings.Timespec.t in
  setf timespec Bindings.Timespec.tv_sec (PosixTypes.Time.of_int64 sec) ;
  setf timespec Bindings.Timespec.tv_nsec (Signed.Long.of_int64 nsec) ;
  timespec
