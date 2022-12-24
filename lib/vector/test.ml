
open Format
open Vector

let () =
  let v = make 0 ~dummy:42 in
  push v 17;
  push v 2;
  assert (length v = 2);
  append v v;
  assert (length v = 4);
  assert (get v 2 = 17)

let () =
  let n = 20 in
  let v = make 1 ~dummy:0 in
  push v 1;
  for i = 2 to n do push v (get v (i-2) + get v (i-1)) done;
  assert (length v = n+1);
  iter (fun x -> printf "%d " x) v; printf "@.";
  assert (get v n = 6765)

(* stack *)

let () =
  let s = create ~dummy:42 in
  push s 1;
  assert (top s = 1);
  push s 2;
  push s 3;
  assert (top s = 3);
  assert (pop s = 3);
  assert (top s = 2);
  assert (pop s = 2);
  assert (top s = 1);
  assert (pop s = 1);
  assert (length s = 0);
  assert (try ignore (top s); false with Empty -> true);
  assert (try ignore (pop s); false with Empty -> true);
  ()

let () =
  let v = make 12 ~dummy:() in
  for i = 0 to 1000 do resize v i done;
  for i = 1000 downto 0 do resize v i done;
  for _ = 1 to 1000 do resize v (Random.int 10_000) done;
  ()

(* swap_contents *)

let () =
  let odd = [ 1; 3; 5 ] and even = [ 2; 4 ] in
  let init_odd = of_list ~dummy:0 odd
  and init_even = of_list ~dummy:0 even in
  swap_contents init_odd init_even;
  assert (to_list init_odd = even);
  assert (to_list init_even = odd)
