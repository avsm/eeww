# Hdr_histogram_ocaml

OCaml bindings to
[Hdr_histogram_c](https://github.com/HdrHistogram/HdrHistogram_c). 

## Hdr_histogram_c

The C port of [HdrHistogram](http://hdrhistogram.org/),
[Hdr_histogram_c](https://github.com/HdrHistogram/HdrHistogram_c), is a
pre-requisite for this library. Here are the steps to install HDR histogram C
library:

```bash
$ git clone https://github.com/HdrHistogram/HdrHistogram_c
$ mkdir _build
$ cd _build
$ cmake ..
$ make
$ make install
```

## Installation

Install `hdr_histogram`:

```bash
$ git clone https://github.com/kayceesrk/hdr_histogram_ocaml
$ cd hdr_histogram_ocaml
$ opam pin add .
```

## macOS Monterey notes

On macOS Monterey 12.2.1, the library is installed in `/usr/local`. If the C
compiler is unable to find the header file `hdr/hdr_histogram.h`, then include
the following path in `C_INCLUDE_PATH`.

```bash
$ export C_INCLUDE_PATH=$C_INCLUDE_PATH:/usr/local/include
```

While running programs linked with `hdr_histogram`, if you see the error

```bash
symbol not found in flat namespace '_hdr_close'
```

then you will need to include the library explicitly using
`DYLD_INSERT_LIBRARIES`. For example:

```bash
$ utop
utop # #require "hdr_histogram";;
Cannot load required shared library dllhdr_histogram_stubs.
Reason: /Users/kc/.opam/5.0.0~alpha0/lib/stublibs/dllhdr_histogram_stubs.so: dlopen(/Users/kc/.opam/5.0.0~alpha0/lib/stublibs/dllhdr_histogram_stubs.so, 0x000A): symbol not found in flat namespace '_hdr_close'.
utop #
^d
$ DYLD_INSERT_LIBRARIES=/usr/local/lib/libhdr_histogram.6.1.1.dylib utop
utop # #require "hdr_histogram";;
utop #
```
