Unreleased
--------------

- Set file descriptors in non-blocking mode
  ([#12](https://github.com/anmonteiro/eio-ssl/pull/12))
  - OpenSSL requires file descriptors to be in non-blocking mode to avoid
    blocking the entire OCaml domain

0.1.0 2021-10-21
--------------

- Initial public release
