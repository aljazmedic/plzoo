(* Parser combinatorji *)

module ListMonad = struct
  let return = Seq.return
  let ( >>= ) x f = Seq.concat_map f x
  let ( let@ ) = ( >>= )
  let fail = Seq.empty
end

module ParserMonad : sig
  type ('token, 'a) t = 'token Seq.t -> ('a * 'token Seq.t) Seq.t

  val return : 'a -> ('token, 'a) t
  val fail : ('token, 'a) t
  val eof : ('token, unit) t
  val get : ('token, 'token) t
  val ( || ) : ('token, 'a) t -> ('token, 'a) t -> ('token, 'a) t
  val ( let* ) : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t
  val ( >>= ) : ('token, 'a) t -> ('a -> ('token, 'b) t) -> ('token, 'b) t
end = struct
  type ('token, 'a) t = 'token Seq.t -> ('a * 'token Seq.t) Seq.t

  let return x inp = Seq.return (x, inp)

  let ( >>= ) (p : ('token, 'a) t) (f : 'a -> ('token, 'b) t) : ('token, 'b) t =
   fun inp -> Seq.flat_map (fun (x, inp') -> (f x) inp') (p inp)

  let ( let* ) = ( >>= )

  (** Fail parser directly fails. *)
  let fail _ = Seq.empty

  (** Gets next token from the stream *)
  let get inp =
    match Seq.uncons inp with
    | None -> Seq.empty
    | Some (t, ts) -> Seq.return (t, ts)

  (** End of stream parser. Succeeds if the input stream is empty. *)
  let eof inp =
    match Seq.uncons inp with
    | None -> Seq.return ((), Seq.empty)
    | Some _ -> Seq.empty

  (** Or. Option to choose from either parse result of [p1] pr [p2] *)
  let ( || ) c1 c2 inp = Seq.append (c1 inp) (c2 inp)
end

open ParserMonad

let first p inp =
  match Seq.uncons (p inp) with
  | None -> Seq.empty
  | Some (x, _) -> Seq.return x

(** Fallback or. Option to choose from either parse result of [p1] pr [p2] *)
let ( ||| ) p1 p2 = first (p1 || p2)

let return_many xs inp = Seq.map (fun x -> (x, inp)) xs

(** Parser that succeeds if the next token is [k'] *)
let kw k' =
  let* x = get in
  if x = k' then return x else fail

(** Concatenation of parsers, returning a pair *)
let ( @@@ ) p1 p2 =
  let* x = p1 in
  let* y = p2 in
  return (x, y)

(** Concatenation of parsers, discarding left *)
let ( <@@@ ) p1 p2 =
  let* _ = p1 in
  let* x = p2 in
  return x

(** Concatenation of parsers, discarding right *)
let ( @@@< ) p1 p2 =
  let* x = p1 in
  let* _ = p2 in
  return x

(** Kleene star *)
let rec iter p =
  (let* x = p in
   let* xs = iter p in
   return @@ Seq.cons x xs)
  || (return @@ Seq.empty)

(** Kleene plus *)
let iter1 p =
  let* x = p in
  let* xs = iter p in
  return @@ Seq.cons x xs

let rec between p = function
  | [] -> assert false
  | [ k ] -> kw k <@@@ return Seq.empty
  | k :: ks ->
      kw k
      <@@@
      let* arg0 = p in
      let* args = between p ks in
      return @@ Seq.cons arg0 args

(* Auxiliary functions *)
let cons_back xs x = Seq.append xs (Seq.return x)

let op_application op args =
  let op_name = Syntax.Var (Operator.name_of_operator op) in
  Seq.fold_left (fun app arg -> Syntax.Apply (app, arg)) op_name args

let rec seq_fold_right f acc sequence =
  match Seq.uncons sequence with
  | None -> acc
  | Some (x, xs) -> f (seq_fold_right f acc xs) x

(* Core of parsing *)

let rec expr (env : Environment.parser_context) e =
  let open ListMonad in
  match e with
  | Presyntax.Var x ->
      if Environment.identifier_present env x then return @@ Syntax.Var x
      else Seq.empty
  | Presyntax.Seq es ->
      let seq_parser = get_graph_parser env in
      let@ e, leftover = seq_parser es in
      if Seq.is_empty @@ leftover then return e else fail
  | Presyntax.Int k -> return @@ Syntax.Int k
  | Presyntax.Bool b -> return @@ Syntax.Bool b
  | Presyntax.Nil ht -> return @@ Syntax.Nil ht
  | Presyntax.Times (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Times (e1, e2)
  | Presyntax.Divide (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Divide (e1, e2)
  | Presyntax.Mod (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Mod (e1, e2)
  | Presyntax.Plus (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Plus (e1, e2)
  | Presyntax.Minus (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Minus (e1, e2)
  | Presyntax.Equal (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Equal (e1, e2)
  | Presyntax.Less (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Less (e1, e2)
  | Presyntax.If (e1, e2, e3) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      let@ e3 = expr env e3 in
      return @@ Syntax.If (e1, e2, e3)
  | Presyntax.Fun (name, ht, e) ->
      let env = env |> Environment.pctx_add_identifier name in
      let@ e = expr env e in
      return @@ Syntax.Fun (name, ht, e)
  | Presyntax.Pair (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Pair (e1, e2)
  | Presyntax.Fst e ->
      let@ e = expr env e in
      return @@ Syntax.Fst e
  | Presyntax.Snd e ->
      let@ e = expr env e in
      return @@ Syntax.Snd e
  | Presyntax.Rec (x, ht, e) ->
      let@ e = expr env e in
      return @@ Syntax.Rec (x, ht, e)
  | Presyntax.Cons (e1, e2) ->
      let@ e1 = expr env e1 in
      let@ e2 = expr env e2 in
      return @@ Syntax.Cons (e1, e2)
  | Presyntax.Match (e, ht, e1, x, y, e2) ->
      let@ e = expr env e in
      let@ e1 = expr env e1 in
      let env2 =
        env
        |> Environment.pctx_add_identifier x
        |> Environment.pctx_add_identifier y
      in
      let@ e2 = expr env2 e2 in
      return @@ Syntax.Match (e, ht, e1, x, y, e2)

and get_graph_parser env : (Presyntax.expr, Syntax.expr) t =
  let g = env.operators in
  let app_parser env =
    let rec parse_app app tail =
      let open ListMonad in
      match Seq.uncons tail with
      | None -> return app
      | Some (arg0, tail) ->
          let@ arg0 = expr env arg0 in
          parse_app (Syntax.Apply (app, arg0)) tail
    in

    let* args = iter1 get in
    match Seq.uncons args with
    | None -> fail
    | Some (head, tail) ->
        return_many
          (let open ListMonad in
           let@ head = expr env head in
           parse_app head tail)
  in

  let rec precedences (ps : Operator.precedence list) inp =
    match ps with
    | [] -> app_parser env inp
    | (_, p) :: ps ->
        let p_up = precedences ps in
        (precedence p p_up ||| p_up) inp

  and inner = function
    | [] -> fail
    | op :: ops ->
        (let* parts = between (precedences g) (Operator.name_parts op) in
         return (op, parts))
        ||| inner ops
        
  and precedence cur_prec p_up =
    let open Operator in
    let inner_fx fx =
      let* op, args = inner @@ op_of_fix fx cur_prec in
      return (op_application op, args)
    in

    (* Closed *)
    (let* (app, args) = inner_fx Closed in
     return @@ app args)

    (* Infix NonAssoc *)
    || (let* arg0 = p_up in
        let* (app, args) = inner_fx (Infix NonAssoc) in
        let* argZ = p_up in
        let args = cons_back (Seq.cons arg0 args) argZ in
        return @@ app args)
        
    (* Postfix/LeftAssoc*)
    || let postLeft =
          (let* (app, args) = inner_fx @@ Postfix in
          return @@ fun first -> app @@ Seq.cons first args)
          ||
          (let* (app, args) = inner_fx @@ Infix LeftAssoc in
          let* argZ = p_up in
          let args = cons_back args argZ in
          return @@ fun first -> app @@ Seq.cons first args)
        in 
        (let* arg0 = p_up in
        let* apps = iter1 postLeft in
        return @@ Seq.fold_left (fun arg app -> app arg) arg0 apps)

    (* Prefix/RightAssoc*)
    || let preRight =
          (let* (app, args) = inner_fx @@ Prefix in
          return @@ fun last -> app @@ cons_back args last)
          ||
          let* arg0 = p_up in
          let* (app, args) = inner_fx @@ Infix RightAssoc in
          return @@ fun last -> app @@ cons_back (Seq.cons arg0 args) last
        in
        (let* apps = iter1 preRight in
        let* argZ = p_up in
        return @@ seq_fold_right (fun app arg -> arg app) argZ apps)

    (* Fail *)
    || fail
  in

  precedences g

let check_success lst =
  let rec fmt_ambigous ambg =
    match Seq.uncons ambg with
    | None -> ""
    | Some (a, tail) ->
        (Printf.sprintf "+ %s\n%s" (Syntax.string_of_expr a))
          (fmt_ambigous tail)
  in
  match Seq.uncons lst with
  | None -> Zoo.error ?kind:(Some "Syntax error") "Could not parse."
  | Some (a, tail) -> (
      match Seq.uncons tail with
      | None -> a
      | _ ->
          Zoo.error ?kind:(Some "Syntax error") "Ambiguous parse:\n%s"
            (fmt_ambigous lst))
