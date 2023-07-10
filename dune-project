(lang dune 3.6)

(name par_incr)

(generate_opam_files true)

(source
 (github ocaml-multicore/par_incr))

(authors "Dipesh Kafle")

(maintainers "Dipesh Kafle")

(license MIT)

(documentation https://ocaml-multicore.github.io/par_incr/par_incr/index.html)

(package
 (name par_incr)
 (synopsis "Parallel Self Adjusting Computation")
 (description "A library for incremental computations, with support for parallel operations")
 (depends ocaml dune domainslib (alcotest :with-test) (mdx :with-test))
 (tags
  (ocaml incremental "multicore-ocaml")
))
(using mdx 0.2)
