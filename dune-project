(lang dune 3.19)

(name logic)

(generate_opam_files true)

(source
 (github leo-leesco/Logic))

(authors "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(maintainers "Léo LEESCO <leo.leesco@eecs.berkeley.edu>")

(package
 (name logic)
 (synopsis "Logic fundamentals : theory, models")
 (description "This repository contains the datastructures corresponding to the INF412 course given at École Polytechnique (Fondements de l'informatique : Logique, modèles et calculs, par O. Bournez)")
 (depends ocaml)
 (tags
  ("logic" "theory" "models" "first-order logic" "structures")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
