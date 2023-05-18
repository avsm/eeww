# EIO-SSL: OpenSSL bindings to [EIO](https://github.com/ocaml-multicore/eio)

A wrapper around [OCaml-SSL][ocaml-ssl] that performs I/O concurrently with
[eio][eio].

To install, do `opam install eio-ssl`.

For documentation, see the [`.mli` file][mli].

[ocaml-ssl]: https://github.com/savonet/ocaml-ssl
[openssl]: https://www.openssl.org/
[mli]: https://github.com/anmonteiro/eio-ssl/blob/master/src/eio_ssl.mli
[eio]: https://github.com/ocaml-multicore/eio


## License & Copyright

This work is based on [lwt_ssl](https://github.com/aantron/lwt_ssl) and shares
the same license. See [LICENSE](./LICENSE) and the headers in the source files.
