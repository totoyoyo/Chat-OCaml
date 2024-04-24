(lang dune 3.14)

(name chat_one_on_one)

(source
 (github ToToYoYo/Chat-OCaml))

(authors "ToTo")
(maintainers "ToTo")
(license MIT)
(generate_opam_files true)

(package
 (name chat_one_on_one)
 (synopsis "A simple one-on-one chat app")
 (description "This is a simple one-on-one chat app written in OCaml.")
 (depends ocaml dune lwt lwt_ppx)
 (tags
  (topics "to describe" your project)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
