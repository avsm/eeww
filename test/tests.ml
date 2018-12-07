let n_of_s = Domain_name.of_string_exn

let basic_preds () =
  Alcotest.(check bool "root is_hostname" true (Domain_name.is_hostname Domain_name.root)) ;
  Alcotest.(check bool "root is no service" false (Domain_name.is_service Domain_name.root)) ;
  Alcotest.(check bool "_tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "_tcp.foo"))) ;
  Alcotest.(check bool "_._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "_._tcp.foo"))) ;
  Alcotest.(check bool "foo._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "foo._tcp.foo"))) ;
  Alcotest.(check bool "f_oo._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "f_oo._tcp.foo"))) ;
  Alcotest.(check bool "foo_._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "foo_._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server._tcp.foo is a service" true
              (Domain_name.is_service (n_of_s ~hostname:false "_xmpp-server._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server._tcp2.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "_xmpp-server._tcp2.foo"))) ;
  Alcotest.(check bool "_xmpp_server._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "_xmpp_server._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server-server._tcp.foo is no service" false
              (Domain_name.is_service (n_of_s ~hostname:false "_xmpp_server-server._tcp.foo"))) ;
  Alcotest.(check bool "_443._tcp.foo is a service" true
              (Domain_name.is_service (n_of_s ~hostname:false "_443._tcp.foo"))) ;
  Alcotest.(check bool "foo is no subdomain of foo.bar" false
              (Domain_name.sub ~subdomain:(n_of_s "foo") ~domain:(n_of_s "foo.bar"))) ;
  Alcotest.(check bool "foo is a subdomain of foo" true
              (Domain_name.sub ~subdomain:(n_of_s "foo") ~domain:(n_of_s "foo"))) ;
  Alcotest.(check bool "bar.foo is a subdomain of foo" true
              (Domain_name.sub ~subdomain:(n_of_s "bar.foo") ~domain:(n_of_s "foo")))

let case () =
  Alcotest.(check bool "foo123.com and Foo123.com are equal" true
              (Domain_name.equal (n_of_s "foo123.com") (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are not equal if case" false
              (Domain_name.equal ~case_sensitive:true
                 (n_of_s "foo123.com") (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo-123.com and com are not equal" false
              (Domain_name.equal (n_of_s "foo-123.com") (n_of_s "com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are equal if case _and_ canonical used on second"
              true
              Domain_name.(equal ~case_sensitive:true
                 (n_of_s "foo123.com") (canonical (n_of_s "Foo123.com")))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are not equal if case _and_ canonical used on first"
              false
              Domain_name.(equal ~case_sensitive:true
                 (canonical (n_of_s "foo123.com")) (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are equal if case _and_ canonical used on both"
              true
              Domain_name.(equal ~case_sensitive:true
                 (canonical (n_of_s "foo123.com")) (canonical (n_of_s "Foo123.com"))))


let p_msg =
  let module M = struct
    type t = [ `Msg of string ]
    let pp ppf (`Msg s) = Fmt.string ppf s
    let equal _ _ = true
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let p_name = Alcotest.testable Domain_name.pp Domain_name.equal

let basic_name () =
  Alcotest.(check (result p_name p_msg) "prepend '_foo' to root is not valid"
              (Error (`Msg "")) (Domain_name.prepend Domain_name.root "_foo")) ;
  Alcotest.(check_raises "prepend_exn '_foo' to root raises"
              (Invalid_argument "invalid host name")
              (fun () -> ignore (Domain_name.prepend_exn Domain_name.root "_foo"))) ;
  Alcotest.(check (result p_name p_msg) "of_strings '_foo' ; 'bar' is not valid"
              (Error (`Msg "")) (Domain_name.of_strings [ "_foo" ; "bar" ])) ;
  Alcotest.(check_raises "of_strings_exn '_foo.bar' raises"
              (Invalid_argument "invalid host name")
              (fun () -> ignore (Domain_name.of_strings_exn [ "_foo" ; "bar" ]))) ;
  Alcotest.(check (result p_name p_msg) "of_string 'foo.bar' is valid"
              (Ok (n_of_s "foo.bar")) (Domain_name.of_string "foo.bar")) ;
  Alcotest.(check p_name "of_array 'foo.bar' is good"
              (n_of_s "foo.bar") (Domain_name.of_array [| "bar" ; "foo" |]))

let fqdn () =
  Alcotest.(check bool "of_string_exn example.com = of_string_exn example.com."
              true
              (Domain_name.equal (n_of_s "example.com") (n_of_s "example.com."))) ;
  Alcotest.(check bool "of_strings_exn ['example' ; 'com'] = of_strings_exn ['example' ; 'com' ; '']"
              true
              Domain_name.(equal
                             (of_strings_exn [ "example" ; "com" ])
                             (of_strings_exn [ "example" ; "com" ; "" ])))

let fqdn_around () =
  let d = n_of_s "foo.com." in
  Alcotest.(check bool "of_string (to_string (of_string 'foo.com.')) works"
              true Domain_name.(equal d (of_string_exn (to_string d)))) ;
  Alcotest.(check bool "of_string (to_string ~trailing:true (of_string 'foo.com.')) works"
              true Domain_name.(equal d (of_string_exn (to_string ~trailing:true d))))

let drop_labels () =
  let res = n_of_s "foo.com" in
  Alcotest.(check p_name "dropping 1 label from www.foo.com is foo.com"
              res
              (Domain_name.drop_labels_exn (Domain_name.of_string_exn "www.foo.com"))) ;
  Alcotest.(check p_name "dropping 2 labels from www.bar.foo.com is foo.com"
              res
              (Domain_name.drop_labels_exn ~amount:2 (Domain_name.of_string_exn "www.bar.foo.com"))) ;
  Alcotest.(check p_name "dropping 1 label from the back www.foo.com is www.foo"
              (Domain_name.of_string_exn "www.foo")
              (Domain_name.drop_labels_exn ~back:true (Domain_name.of_string_exn "www.foo.com"))) ;
  Alcotest.(check p_name "prepending 1 and dropping 1 label from foo.com is foo.com"
              res
              (Domain_name.drop_labels_exn (Domain_name.prepend_exn (Domain_name.of_string_exn "foo.com") "www"))) ;
  Alcotest.(check p_name "prepending 1 and dropping 1 label from foo.com is foo.com"
              res
              (Domain_name.drop_labels_exn (Domain_name.prepend_exn (Domain_name.of_string_exn "foo.com") "www"))) ;
  Alcotest.(check (result p_name p_msg)
              "dropping 10 labels from foo.com leads to error"
              (Error (`Msg ""))
              (Domain_name.drop_labels ~amount:10 (Domain_name.of_string_exn "foo.com")))

let tests = [
  "basic predicates", `Quick, basic_preds ;
  "basic name stuff", `Quick, basic_name ;
  "case", `Quick, case ;
  "fqdn", `Quick, fqdn ;
  "fqdn around", `Quick, fqdn_around ;
  "drop labels", `Quick, drop_labels ;
]

let suites = [
  "domain names", tests ;
]

let () = Alcotest.run "domain name tests" suites
