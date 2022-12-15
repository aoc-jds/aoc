(lang dune 2.9)

(name aoc)

(generate_opam_files true)

(source
 (github aoc-jds/aoc))

(authors "Jesse Sullivan")

(maintainers "Jesse Sullivan")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "Advent of Code - OCaml")
 (description "An implementation of the Advent of Code in OCaml")
 (depends ocaml dune fmt alcotest ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
