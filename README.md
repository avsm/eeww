[API reference](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html)

# **domain-local-await** &mdash; Scheduler independent blocking

This library provides a scheduler independent blocking mechanism.

> **NOTE**: Unless you are e.g. working on implementing an effects based
> scheduler or lock-free algorithms, then this is probably not what you are
> looking for. This is a low level mechanism intended for internal use by higher
> level libraries for concurrent programming and is likely to be replaced by an
> official standard blocking mechanism in the future.

The idea behind domain local await (DLA) is to allow one to create blocking
operations that can work with any scheduler that provides the mechanism by
[registering a function](https://ocaml-multicore.github.io/domain-local-await/doc/domain-local-await/Domain_local_await/index.html#val-using)
for the current domain (or, if desired, the current systhread within a domain)
to support blocking while the scheduler is running. In addition, domain local
await provides a default implementation that works with plain domains and
systhreads.

## References

DLA is used to implement blocking operations by the following libraries:

- [kcas](https://github.com/ocaml-multicore/kcas)

## Example: Awaitable atomic locations

Let's implement a simple awaitable atomic location abstraction. First we need
the domain local await library:

```ocaml
# #require "domain-local-await"
```

An awaitable location contains both the current value of the location and a list
of awaiters, which are just `unit -> unit` functions:

```ocaml
# type 'a awaitable_atomic = ('a * (unit -> unit) list) Atomic.t
type 'a awaitable_atomic = ('a * (unit -> unit) list) Atomic.t
```

The constructor of awaitable locations just pairs the initial value with an
empty list of awaiters:

```ocaml
# let awaitable_atomic v : _ awaitable_atomic = Atomic.make (v, [])
val awaitable_atomic : 'a -> 'a awaitable_atomic = <fun>
```

Operations that modify awaitable locations, like `fetch_and_add`, need to call
the awaiters to wake them up after a successful modification:

```ocaml
# let rec fetch_and_add x n =
    let (i, awaiters) as was = Atomic.get x in
      if Atomic.compare_and_set x was (i+n, []) then begin
          List.iter (fun awaiter -> awaiter ()) awaiters;
          i
        end
      else
        fetch_and_add x n
val fetch_and_add : (int * (unit -> unit) list) Atomic.t -> int -> int =
  <fun>
```

We can also have read-only operations, like `get_as`, that can be used to await
for an awaitable location to have a specific value:

```ocaml
# let rec get_as fn x =
    let (v, awaiters) as was = Atomic.get x in
    match fn v with
    | Some w -> w
    | None ->
      let t = Domain_local_await.prepare_for_await () in
      if Atomic.compare_and_set x was (v, t.release :: awaiters) then
        match t.await () with
        | () -> get_as fn x
        | exception cancelation_exn ->
          let rec cleanup () =
            let (w, awaiters) as was = Atomic.get x in
            if v == w then
              let awaiters = List.filter ((!=) t.release) awaiters in
              if not (Atomic.compare_and_set x was (w, awaiters))
              then cleanup ()
          in
          cleanup ();
          raise cancelation_exn
      else
        get_as fn x
val get_as : ('a -> 'b option) -> ('a * (unit -> unit) list) Atomic.t -> 'b =
  <fun>
```

Notice that we carefully cleaned up in case the `await` was canceled.

We could, of course, also have operations that potentially await for the
location to have an acceptable value before attempting modification. Let's leave
that as an exercise.

To test awaitable locations, let's first create a location:

```ocaml
# let x = awaitable_atomic 0
val x : int awaitable_atomic = <abstr>
```

And let's then create a thread

```ocaml
# #thread
```

that awaits until the value of the location has changed and then modifies the
value of the location:

```ocaml
# let a_thread =
    ()
    |> Thread.create @@ fun () ->
       get_as (fun x -> if x = 0 then None else Some ()) x;
       fetch_and_add x 21 |> ignore
val a_thread : Thread.t = <abstr>
```

The other thread is now awaiting for the initial modification:

```ocaml
# assert (0 = fetch_and_add x 21)
- : unit = ()
```

And we can await for the thread to perform its modification:

```ocaml
# get_as (fun x -> if x <> 21 then Some x else None) x;
- : int = 42
```

Let's then finish by joining with the other thread:

```ocaml
# Thread.join a_thread
- : unit = ()
```

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release` to publish the new `VERSION`.
4. Run `./update-gh-pages-for-tag VERSION` to update the online documentation.
