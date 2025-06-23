(lang dune 3.18)

(name ocaml-playground)

(generate_opam_files true)

(source
 (github username/reponame))

(license MIT)

(using directory-targets 0.1)

(package
 (name ocaml-playground)
 (synopsis "OCaml playground")
 (authors "niten01 <https://github.com/niten01>")
 (maintainers "niten01 <https://github.com/niten01>")
 (depends ocaml
          raylib
          raygui)
 (tags
  ("game" "3d" "raylib"))
 )


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
