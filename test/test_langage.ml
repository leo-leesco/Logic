open Logic.Langage

module Propositionnel = struct
  open Propositionnel

  let to_string = to_string ~string_of_a:Fun.id

  let expr =
    Equiv
      (Not (And (Var "a", Var "b")), Or (Implies (Var "c", Var "d"), Var "e"))

  let () = print_endline (to_string expr)
  let () = print_endline (to_string (to_cnf expr))
end
