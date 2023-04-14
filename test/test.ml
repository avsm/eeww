let test_all_threads_are_woken_up () =
  let n = ref 2 in

  let barrier = Domain_local_await.prepare_for_await () in

  let awaiters = ref [] in

  let threads =
    List.init !n @@ fun _ ->
    ()
    |> Thread.create @@ fun () ->
       let t = Domain_local_await.prepare_for_await () in
       awaiters := t.release :: !awaiters;
       decr n;
       if !n = 0 then barrier.release ();
       t.await ()
  in

  barrier.await ();

  !awaiters |> List.iter (fun awaiter -> awaiter ());

  threads |> List.iter Thread.join

let () =
  test_all_threads_are_woken_up ();
  Domain_local_await.per_thread (module Thread);
  test_all_threads_are_woken_up ();
  ()
