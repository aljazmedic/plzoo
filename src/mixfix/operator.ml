(* Operators *)

type associativity = LeftAssoc | RightAssoc | NonAssoc

let string_of_assoc = function
  | LeftAssoc -> "left"
  | RightAssoc -> "right"
  | NonAssoc -> "non"

(** The type of fixities. *)

type fixity = Prefix | Postfix | Infix of associativity | Closed

let string_of_fixity = function
  | Prefix -> "prefix"
  | Postfix -> "postfix"
  | Infix LeftAssoc -> "infixl"
  | Infix RightAssoc -> "infixr"
  | Infix NonAssoc -> "infix"
  | Closed -> "closed"

(** The type of operators. *)

type t = {
  tokens: string list;
  fx : fixity;
}

(** Returns normalized name of operator.*)
let name_of_operator {fx;tokens} =
  let tokens = String.concat "_" tokens in
  match fx with
  | Closed -> tokens
  | Prefix -> tokens ^ "_"
  | Postfix -> "_" ^ tokens
  | Infix _ -> "_" ^ tokens ^ "_"

let string_of_op ({fx;tokens} as op) = 
  (name_of_operator op) ^ " (fx:" ^ (string_of_fixity fx) ^ ")"

  
(* Operator precedence relationship *)

(** Precedence level of a group of operators. In [(a,b)] higher [a] means higher precedence *)
type precedence = int * t list

let string_of_precedence (p, lst) =
  Printf.sprintf "Prec. %d:\n + %s" p (
    String.concat "\n + " (List.map string_of_op lst)
    )

(** Totally orderered [precedence] list *)
type graph = precedence list

let empty_graph = []

let rec string_of_graph (g:graph) =
  match g with
  | [] -> "<no operators defined>"
  | [p] -> string_of_precedence p
  | p::g -> string_of_precedence p ^ "\n" ^ (string_of_graph g)

(** Returns the unfolding trees that has higher precedence than given [p].*)
let rec sucs (p:precedence) = function
  | [] -> []
  | (x, _)::_ as r when x >= (fst p) -> r
  | _::rest -> sucs p rest


(** Find the an already-defined operator that has one of its tokens in the given list. *)
let find_duplicate_token tokens0 = 
  let ( let& ) = Option.bind in 
    let token_in_namelist = List.find_opt (fun token -> List.exists (( = ) token) tokens0) in
    let token_in_operator (operator:t) = 
      let& token = token_in_namelist operator.tokens in
      Some (token, operator) in
    let token_in_precedence (prec, operators) =
      let& (op, token) =  List.find_map token_in_operator operators in
      Some (prec, op, token) in
    List.find_map token_in_precedence

(* Make sure the graph is always sorted! *)
(** Graph insert. It performs sorted insertion of operator *)
let add_operator (prec, operator) (state: graph): graph = 
  (* Check for duplicate tokens *)
  match state |> find_duplicate_token operator.tokens  with
    | Some (prec, token, op2) -> 
        Zoo.error ?kind:(Some "Operator error") "Duplicate token '%s' in operator %s. @." token (string_of_op op2)
    | None ->
  (* End duplicate check *)
  let rec add_operator (prec, operator) (state:graph) = 
  match state with
  | [] -> [(prec, [operator])]
  | (prec', lst) :: tail -> 
    if prec = prec' then
      (prec', operator::lst) :: tail
    else
      if prec' < prec then
        (prec', lst) :: add_operator (prec, operator) tail
      else
        (prec, [operator]) :: state
  in
    add_operator (prec, operator) state


let determine_fixity assoc s =
  let first_char = String.get s 0 and last_char = String.get s (String.length s - 1) in
  match (first_char, last_char, assoc) with
  | ('_','_', Presyntax.LeftAssoc) -> Infix LeftAssoc
  | ('_','_', Presyntax.RightAssoc) -> Infix RightAssoc
  | ('_','_', Presyntax.NonAssoc) -> Infix NonAssoc
  | ('_',_, Presyntax.NonAssoc) -> Postfix
  | (_, '_', Presyntax.NonAssoc) -> Prefix
  | (_, _, Presyntax.NonAssoc) -> Closed
  | _ -> 
    Zoo.error ?kind:(Some "Operator error") "Cannot provide associativity for postfix, prefix or closed operators. Use 'mixfix' instead." 

let create assoc name =
  if not (String.contains_from name 0 '_') then
    Zoo.error ?kind:(Some "Operator error") "Operator must contain at least one underscore"
    else
      let tokens =  name |> String.split_on_char '_' |> List.filter (fun x -> x <> "") in
  let fx = determine_fixity assoc name in
    match tokens with 
    | [] -> Zoo.error ?kind:(Some "Operator error") "Empty operator"
    | tokens -> {tokens ; fx }
