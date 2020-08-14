module Bindings = Kqueue_stubs.Definition (Kqueue_generated_stubs)
module Constants = Kqueue_constants.Definition (Kqueue_generated_constants)

let unsafe_fd_to_int : Unix.file_descr -> int = Obj.magic
