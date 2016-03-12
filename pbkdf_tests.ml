let test_pbkdf1 ~hash ~password ~salt ~count ~dk_len ~dk () =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and dk = of_hex dk
  and password = Cstruct.of_string password
  in
  (fun () ->
     let edk = Pbkdf.pbkdf1 ~hash ~password ~salt ~count ~dk_len in
     let sedk = Cstruct.to_string edk
     and sdk = Cstruct.to_string dk
     in
     Alcotest.check Alcotest.string "PBKDF test" sedk sdk)

let test_pbkdf1_invalid_arg ~hash ~password ~salt ~count ~dk_len ~msg () =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and password = Cstruct.of_string password
  in
  Alcotest.check_raises
    msg
    (Invalid_argument "")
    (fun () -> ignore (Pbkdf.pbkdf1 ~hash ~password ~salt ~count ~dk_len))

(* Taken from http://www.di-mgt.com.au/cryptoKDFs.html *)
let test1 =
  test_pbkdf1
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:16
    ~dk:"dc19847e05c64d2faf10ebfb4a3d2a20"
    ()

let test2 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA256
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:16
    ~msg:"Invalid hash"

let test3 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb"
    ~count:1000
    ~dk_len:16
    ~msg:"Salt too short"

let test4 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb0600"
    ~count:1000
    ~dk_len:16
    ~msg:"Salt too long"

let test5 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb0600"
    ~count:1000
    ~dk_len:16
    ~msg:"Salt too long"

let test6 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:(-1)
    ~dk_len:16
    ~msg:"Invalid count"

let test7 =
  test_pbkdf1_invalid_arg
    ~hash:`SHA1
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:24
    ~msg:"Invalid derived key length"


let tests = [
  "Test Case 1", `Quick, test1;
  "Test Case 2", `Quick, test2;
  "Test Case 3", `Quick, test3;
  "Test Case 4", `Quick, test4;
  "Test Case 5", `Quick, test5;
  "Test Case 6", `Quick, test6;
  "Test Case 7", `Quick, test7;
]

let () = Alcotest.run "PBKDF Tests" [ "Sample tests", tests ]
