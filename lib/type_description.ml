open Ctypes

module Types (F : Ctypes.TYPE) = struct
(*   open F *)

  type hdr_histogram
  let hdr_histogram : hdr_histogram structure typ = structure "hdr_histogram"
end
