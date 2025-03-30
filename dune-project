(lang dune 3.16)

(using menhir 3.0)

(name perkelang)

(generate_opam_files true)

(source
 (github Alex23087/Perkelang))

(authors "Alex23087" "mell-o-tron")

(maintainers "Alex23087" "mell-o-tron")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name perkelang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
