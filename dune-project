(lang dune 3.6)

(name opstic)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name opstic)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (ppxlib (>= :"0.19.0")) ppx_deriving (ounit :with-test) ppx_inline_test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
