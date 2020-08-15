open Base
open Base_quickcheck

module F = struct
  type flag = Kqueue.Flag.t =
    | Add
    | Enable
    | Disable
    | Dispatch
    | Delete
    | Receipt
    | Oneshot
    | Clear
    | EOF
    | Error
  [@@deriving sexp, compare, quickcheck, equal]

  type t = flag list [@@deriving sexp, compare, quickcheck]
end

let test_flags () =
  Test.run_exn
    (module F)
    ~f:(fun flags ->
      let uint = Kqueue.Flag.flags_to_uint flags in
      (* dedup and sort since list can contain duplicate flags and `flag lor
         flag = flag` *)
      let flags = List.dedup_and_sort ~compare:F.compare_flag flags in
      let flags' =
        Kqueue.Flag.flags_of_uint uint |> List.sort ~compare:F.compare_flag
      in
      [%test_result: F.t] ~expect:flags flags')

let () = test_flags ()
