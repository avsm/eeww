let test_pbkdf1 ~hash ~password ~salt ~count ~dk_len ~dk () =
  let open Nocrypto.Uncommon.Cs in
  let salt = of_hex salt
  and dk = of_hex dk
  and password = Cstruct.of_string password
  in
  (fun () ->
     let edk = Pbkdf.pbkdf1 ~hash ~password ~salt ~count ~dk_len ~hlen:20 in
     let sedk = Cstruct.to_string edk
     and sdk = Cstruct.to_string dk
     in
     Alcotest.check Alcotest.string "PBKDF test" sedk sdk)
  
let test1 =
  test_pbkdf1
    ~hash:Nocrypto.Hash.SHA1.digest
    ~password:"password"
    ~salt:"78578e5a5d63cb06"
    ~count:1000
    ~dk_len:16
    ~dk:"dc19847e05c64d2faf10ebfb4a3d2a20"
    ()

let tests = [
  "Test 1", `Quick, test1 ;
]

let () = Alcotest.run "PBKDF Tests" [ "Sample tests", tests ]
