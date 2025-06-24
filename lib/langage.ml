(** représente des parenthésages valides *)
module Dyck = struct
  type t = Empty | Par of t | Concat of t * t
end

(** décrit les formules propositionnelles formées à partir de l'alphabet décrit
    par le type ['a] *)
module Propositionnel = struct
  type 'a t =
    | Var of 'a
    | Not of 'a t
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Implies of 'a t * 'a t
    | Equiv of 'a t * 'a t

  type 'a valuation = 'a -> bool

  let rec to_string ~(string_of_a : 'a -> string) = function
    | Var v -> string_of_a v
    | Not p -> "¬(" ^ to_string ~string_of_a p ^ ")"
    | And (f, g) ->
        "(" ^ to_string ~string_of_a f ^ " ∧ " ^ to_string ~string_of_a g ^ ")"
    | Or (f, g) ->
        "(" ^ to_string ~string_of_a f ^ " ∨ " ^ to_string ~string_of_a g ^ ")"
    | Implies (f, g) ->
        "(" ^ to_string ~string_of_a f ^ " ⟹ " ^ to_string ~string_of_a g ^ ")"
    | Equiv (f, g) ->
        "(" ^ to_string ~string_of_a f ^ " ⟺ " ^ to_string ~string_of_a g ^ ")"

  let rec ( @@ ) (v : 'a valuation) = function
    | Var p -> v p
    | Not p -> not (v @@ p)
    | And (p, q) -> (v @@ p) && (v @@ q)
    | Or (p, q) -> (v @@ p) || (v @@ q)
    | Implies (p, q) -> (not (v @@ p)) || (v @@ q)
    | Equiv (p, q) -> v @@ p = v @@ q

  let rec ( .![]<- ) (f : 'a t) (p : 'a) (g : 'a t) =
    match f with
    | Var v -> if v = p then g else f
    | Not h -> Not (h.![p] <- g)
    | And (f1, f2) -> And ((f1.![p] <- g), (f2.![p] <- g))
    | Or (f1, f2) -> Or ((f1.![p] <- g), (f2.![p] <- g))
    | Implies (f1, f2) -> Implies ((f1.![p] <- g), (f2.![p] <- g))
    | Equiv (f1, f2) -> Equiv ((f1.![p] <- g), (f2.![p] <- g))

  let rec (to_cnf
           [@deprecated "This function is buggy and will loop infinitely"]) =
    function
    | Var _ as f -> f
    | And (f, g) -> And (to_cnf f, to_cnf g)
    | Implies (f, g) -> to_cnf (Or (Not f, g))
    | Equiv (f, g) -> And (to_cnf (Implies (f, g)), to_cnf (Implies (g, f)))
    | Not p -> (
        match p with
        | Var _ as f -> Not f
        | Not f -> to_cnf f
        | And (f, g) -> to_cnf (Or (Not f, Not g))
        | Or (f, g) -> And (to_cnf (Not f), to_cnf (Not g))
        | Implies (f, g) -> And (to_cnf f, to_cnf (Not g))
        | Equiv (f, g) ->
            to_cnf (Or (Not (Implies (f, g)), Not (Implies (g, f)))))
    | Or (f, g) -> to_cnf (Not (And (Not f, Not g)))
end
