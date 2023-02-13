(** kqueue(): Kqueue is a scalable event notification interface available on macOS and
    various BSD systems (FreeBSD, OpenBSD, etc).

    Consult the kqueue manpages to see the full list of functionality:

    - {{:https://www.freebsd.org/cgi/man.cgi?kqueue} FreeBSD}
    - {{:https://man.openbsd.org/kqueue.2} OpenBSD}
    - {{:https://opensource.apple.com/source/xnu/xnu-792/bsd/man/man2/kqueue.2} macOS} *)
include Kqueue_intf.S
