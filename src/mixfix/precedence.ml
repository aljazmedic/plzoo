
type name_parts = string list
type t = int * Syntax.operator list
type graph = t list

let string_of_precedence (p, lst) =
  Printf.sprintf "Prec. %d:\n + %s" p (
    String.concat "\n + " (List.map Syntax.string_of_op lst)
  )

let rec string_of_graph (g:graph) =
  match g with
  | [] -> "<no operators defined>"
  | [p] -> string_of_precedence p
  | p::g -> string_of_precedence p ^ "\n" ^ (string_of_graph g)

let rec sucs (p:t) = function
  | [] -> []
  | (x, _)::_ as r when x >= (fst p) -> r
  | _::rest -> sucs p rest

let empty_graph = []

(* Just a helper for more imperative code *)
let ( let& ) = Option.bind

(** Find the an already-defined operator that has one of its tokens in the given list. *)
let find_duplicate_token tokens0 =
  let token_in_list = List.find_opt (fun token -> List.exists (( = ) token) tokens0) in
  let token_in_operator = List.find_map (
    fun (operator: Syntax.operator) -> 
    let& token = token_in_list operator.tokens in
    Some (token, operator)
  ) in
  List.find_map (fun (prec, operators) -> 
    let& (op, token) = token_in_operator operators in
    Some (prec, op, token))

let rec update_precedence (operator:Syntax.operator) = function
  | [] -> [(operator.fx, [operator.tokens])]
  | (fx, lst) :: tail when fx = operator.fx -> 
    (fx, operator.tokens :: lst) :: tail
  | head :: tail -> head :: update_precedence operator tail

(* Make sure the graph is always sorted! *)
let rec add_operator (state: graph) prec operator : graph = 
  match state with
  | [] -> [(prec, [operator])]
  | (prec', lst) :: tail -> 
    if prec = prec' then
      (prec', operator::lst) :: tail
    else
      if prec' < prec then
        (prec', lst) :: add_operator tail prec operator
      else
        (prec, [operator]) :: state
