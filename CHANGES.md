### 1.1.0 (2016-01-27)

* Add a new top-level interface `module Mirage_flow`. Existing `module Fflow`
  is still present.
* Add `Mirage_flow.copy` to copy all the data in a flow to another
* Add `Mirage_flow.proxy` to copy data bidirectionally between two flows

### 1.0.3 (2015-07-29)

* Support lwt 2.5.0

### 1.0.2 (2015-06-30)

* Add explicit dependency to OUnit

### 1.0.1 (2015-04-28)

* Add `Fflow.error_message` to satisfay `mirage-types.2.3.0`

### 1.0.0 (2015-02-26)

* Add `Fflow` (functional flows)
* Add `Lwt_io_flow` to convert between Mirage and Lwt flows
