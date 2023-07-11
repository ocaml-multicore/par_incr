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
 (description "A library for incremental computation, with support for parallelism")
 (depends
  (ocaml (>= "5.0"))
  (dune (>= "3.6"))
  (domainslib (and (>= "0.5.0") :with-test ))
  (alcotest (and (>= "1.7.0") :with-test ))
  (mdx (and (>= "2.3.0") :with-test ))
  (current_incr (and (>= "0.6.1") :with-test))
  (incremental (and (>= "0.15.0") :with-test)))

 (tags
  (ocaml incremental "multicore-ocaml")
))
(using mdx 0.2)
