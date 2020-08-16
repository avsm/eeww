# kqueue-ml

OCaml bindings to [kqueue](https://en.wikipedia.org/wiki/Kqueue)

Timer example using kqueue:

```ocaml
#include "example/timer.ml"
```

## Caveats

This is mostly tested on macOS. At the moment the constant values in the bindings only refer to the values that are
a subset of kqueue constants available on macOS and freebsd.
