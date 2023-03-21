let cookie_size = 37

(* bytes are in network byte order *)
let wire_value_of_state s =
  let buf = Cstruct.create 1 in
  Cstruct.set_uint8 buf 0 (State.to_int s);
  buf

let state_of_wire_value c =
  Cstruct.get_uint8 c 0 |> State.of_int

(* TODO json protocol *)
(* We get messages like
   {"tcp":true,"omit":0,"time":10,"num":0,"blockcount":0,"parallel":1,"len":131072,"pacing_timer":1000,"client_version":"3.13"} *)

let parse_params j =
  Yojson.Safe.from_string j

let params_length v =
  Yojson.Safe.Util.(v |> member "len" |> to_int)

let params_parallel v = 
  Yojson.Safe.Util.(v |> member "parallel" |> to_int)
