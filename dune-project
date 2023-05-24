(lang dune 3.6)

(name par_incr)

(generate_opam_files true)

(source
 (github tarides/par_incr))

(authors "Dipesh Kafle")

(maintainers "Dipesh Kafle")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name par_incr)
 (synopsis "Parallel Self Adjusting Computation")
 (description "Parallel version of the incremental library")
 (depends ocaml dune domainslib (alcotest :with-test))
 (tags
  (ocaml incremental "multicore-ocaml")
))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
