# Hdr_histogram_ocaml

OCaml bindings to
[Hdr_histogram_c](https://github.com/HdrHistogram/HdrHistogram_c).

## Hacking notes

Here are the steps to install HDR histogram C library:

```bash
$ git clone https://github.com/HdrHistogram/HdrHistogram_c
$ mkdir __build
$ cd __build
$ cmake ..
$ make
$ make install
```

The library is installed in `/usr/local`.

On macOS Monterey,
```bash
$ export C_INCLUDE_PATH=/usr/local/include
```
is needed to let the C compiler find the header files.

Install `hdr_histogram`:

```bash
$ git clone https://github.com/kayceesrk/hdr_histogram_ocaml
$ cd hdr_histogram_ocaml
$ opam pin add .
```

On macOS Monterey, running the executable which may use `hdr_histogram` library
may need to point out where the library is. For example,
```bash
$ DYLD_INSERT_LIBRARIES=/usr/local/lib/libhdr_histogram.6.1.1.dylib utop
utop # #require "hdr_histogram";;
```
works as expected.
