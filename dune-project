(lang dune 3.7)

(generate_opam_files true)

(package
 (name linre)
 (depends
  (ocaml (>= "5.4.0"))
  (dune (>= "3.0"))
  (ocamlformat :with-test)))