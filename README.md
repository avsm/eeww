ocaml-dispatch -- bindings to Apple's Grand Central Dispatch
------------------------------------------------------------
*Status: Experimental & WIP*
 
These are OCaml bindings for Apple's Grand Central Dispatch (GCD) library.

This project is heavily influenced by [ocaml-uring](https://github.com/ocaml-multicore/ocaml-uring/). In particular the copying tests (framework and `lwtcp`) are taken directly from that repository. (TODO: attribute this properly in the code).

## Usage

```ocaml
# #require "gcd"
```

GCD uses queues to handle asynchronous and concurrent code. Pushing items of work to a queue and adding a handler to be called upon completion typically on a different thread.

```ocaml
# let q = Dispatch.Queue.create ()
val q : Dispatch.Queue.t = <abstr>
```

We can submit things to do to `q` asynchronously using `Dispatch.async`, this will not block, so will exit before anything happens! To prevent this, we can enqueue a synchronous call after an async call which *does* block until the completion handler is called. 

```ocaml
# Dispatch.async q (fun () -> print_endline "Hello");
  Dispatch.sync q (fun () -> print_endline "World")
Hello
World
- : unit = ()
```

Now in actual fact nothing particularly concurrent is happening here. OCaml has a single-threaded runtime, so any callback will have to register itself with the runtime and acquire the runtime lock. Where this does help is with async IO because the actual reads and writes which typically block on IO can be done truly asynchronously and it is only our completion handlers that need to do the runtime acquiring dance.

First a helper function which takes a function which returns a `Dispatch.Group.t` (a semaphore-like data-structure). We will wait indefinitely for this group to be left.

```ocaml
# let with_group f =  
    Dispatch.Group.wait (f ()) (Dispatch.Time.forever ()) |> ignore
val with_group : (unit -> Dispatch.Group.t) -> unit = <fun>
```

Next a copy function, for every `read` of data we also `write` that data out.

```ocaml
# let cp in_io out_io () = 
    let group = Dispatch.Group.create () in 
      Dispatch.Group.enter group;
      Dispatch.Io.with_read 
        ~off:0 ~length:max_int ~queue:q 
          ~f:(fun ~err:_ ~finished data -> 
              if finished then Dispatch.Group.leave group
              else (
                print_int @@ Dispatch.Data.size data;
                Dispatch.Group.enter group;
                Dispatch.Io.with_write ~off:0 ~data ~queue:q
                  ~f:(fun ~err:_ ~finished:_ _ -> Dispatch.Group.leave group)
                out_io)) in_io;
      group
val cp : Dispatch.Io.t -> Dispatch.Io.t -> unit -> Dispatch.Group.t = <fun>
```

Next we run the function using the `LICENSE.md` and printing to standard out.

```ocaml
# let () =
    let in_io = Dispatch.Io.(create Stream (Fd.of_unix @@ Unix.(openfile "LICENSE.md" [ O_RDONLY ]) 0)) q in 
    let out_io = Dispatch.Io.(create Stream Fd.stdout q) in
    try
      with_group (cp in_io out_io);
       Dispatch.Io.close in_io;
       Dispatch.Io.close out_io
    with _ -> Dispatch.Io.close in_io; Dispatch.Io.close out_io
/*
 * Copyright (C) 2020-2021 Patrick Ferris
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
```
