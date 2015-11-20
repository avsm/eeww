v1.1.0 2015-11-20 Cambridge (UK)
--------------------------------

- Add support for the East Asian width property (`Uucp.Break.east_asian_width`).
- Add the non-normative, heuristic function `Uucp.Break.tty_width_hint`.
  Thanks to David Kaloper for the contribution.

v1.0.0 2015-06-17 Cambridge (UK)
--------------------------------

- Updated for Unicode 8.0.0
  Incompatible release, new variants cases are introduced, see commit
  adbb5efc036 for details.

v0.9.1 2014-12-23 Cugy (VD)
---------------------------

- Add access to the `Line_break`, `Grapheme_cluster_break`, `Word_break` and
  `Sentence_break` Unicode properties. See the `Uucp.Break` module.
- Improvements and fixes to the minimal Unicode Introduction. 


v0.9.0 2014-06-28 Cambridge (UK)
-------------------------------

First release. Part of the work was sponsored by OCaml Labs.
