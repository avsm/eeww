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
  Alcotest.(check bool "foo is no subdomain of foo.bar" false
              (Domain_name.sub ~subdomain:(n_of_s "foo") ~domain:(n_of_s "foo.bar")))

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
              Domain_name.(equal
                             (of_string_exn "example.com")
                             (of_string_exn "example.com."))) ;
  Alcotest.(check bool "of_strings_exn ['example' ; 'com'] = of_strings_exn ['example' ; 'com' ; '']"
              true
              Domain_name.(equal
                             (of_strings_exn [ "example" ; "com" ])
                             (of_strings_exn [ "example" ; "com" ; "" ])))

let drop_labels () =
  let res = Domain_name.of_string_exn "foo.com" in
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
              (Domain_name.drop_labels_exn (Domain_name.prepend_exn (Domain_name.of_string_exn "foo.com") "www")))

let tests = [
  "basic predicates", `Quick, basic_preds ;
  "basic name stuff", `Quick, basic_name ;
  "fqdn", `Quick, fqdn ;
  "drop labels", `Quick, drop_labels ;
]

let suites = [
  "domain names", tests ;
]

let () = Alcotest.run "domain name tests" suites
