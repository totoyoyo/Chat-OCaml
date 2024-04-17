(lang dune 3.14)

(name chat_one_on_one)

; (generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(generate_opam_files)

(package
 (name chat_one_on_one)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune lwt lwt_ppx)
 (tags
  (topics "to describe" your project)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
