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
  print_endline @@ "Waiting for token " ^ (Presyntax.string_of_expr k') ;
  if x = k' then (print_endline "Found!" ; return x )
  else fail

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

(* Core of parsing *)

let rec expr (env : Environment.parser_context) e =
  let open ListMonad in
  match e with
  | Presyntax.Var x ->
      if Environment.identifier_present env x then return @@ Syntax.Var x
      else Seq.empty
  | Presyntax.Seq es ->
      let seq_parser = get_graph_parser env in
      let@ (e, leftover) = seq_parser es in
      if Seq.is_empty @@ leftover then
        return e
      else fail
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
  let app_parser =
    let open ListMonad in
  let rec parse_arguments args =
    match Seq.uncons args with
    | None -> return Seq.empty (* equivalent to fail *)
    | Some (arg0 ,args) ->
      let@ arg0 = expr env arg0 in
      let@ args = parse_arguments args in
      return @@ Seq.cons arg0 args
  in
  let parse_application (presyntaxl : Presyntax.expr Seq.t) =
    match Seq.uncons presyntaxl with
    | None -> fail
    | Some (h, tail) ->
      let@ h = expr env h in
      let@ args = parse_arguments tail in
      return @@ Syntax.make_app h args
  in
  let* args = iter1 get in
  let applications = parse_application args in
  (
    (* print_endline @@ "Parsed: " ^ (String.concat "," (List.of_seq @@ (Seq.map Syntax.string_of_expr applications)) ); *)
  return_many applications)
  in
  let rec get_subgraph_parser (g : Operator.graph) inp =
    let rec prec_lvl_parser prec_up operators inp =
      
      let operator_parser op =
        let prec_same = prec_lvl_parser prec_up operators in
        let op_name = Syntax.Var (Operator.name_of_operator op) in (* e.g. A_B_ *)
        let op_tokens = (List.map (fun x -> Presyntax.Var x) op.tokens) in (* e.g. [A; B] *)

        let app_args =
          Seq.fold_left (fun app arg -> Syntax.Apply (app, arg)) op_name
        in

        let rec app_fold_left arg0 aargs =
          Seq.fold_left
            (fun arg0i argsi -> app_args @@ Seq.cons arg0i argsi)
            arg0 aargs
        and app_fold_right aargs argZ =
          match Seq.uncons aargs with
          | None -> argZ
          | Some (argsi, tail) ->
              app_args @@ cons_back argsi @@ app_fold_right tail argZ
        in

        let between_parser =
          between (get_subgraph_parser g) op_tokens
        in
        match op with
        | { fx = Closed; _ } ->
            (* A_B *)
            let* args = between_parser in
            return @@ app_args args

        | { fx = Postfix; _ } ->
            (* (sA_B)A_B *)
            let* head = prec_up in
            let* tails = iter1 between_parser in
            return @@ app_fold_left head tails
        
        | { fx = Prefix; _ } ->
            (* A_B(A_Bs) *)
            let* head_apps = iter1 between_parser in
            let* appZ = prec_up in
            return @@ app_fold_right head_apps appZ

        | { fx = Infix NonAssoc; _ } ->
            (* sA_B_ -> First token has to be of upper parsing level.  *)
            let* arg0 = prec_up in
            let* mid_args = between_parser in
            let* argZ = prec_up in
            let args = cons_back (Seq.cons arg0 mid_args) argZ in
            return @@ app_args args

        | { fx = Infix LeftAssoc; _ } ->
            (* ((sA_Bs)A_Bs)A_Bs -> First token has to be of upper parsing level.  *)
            let* arg0 = prec_up in
            let* tail_apps =
              iter1
                (let* args_i = between_parser in
                 let* argZ_i = prec_up in
                 return @@ cons_back args_i argZ_i)
            in
            return @@ app_fold_left arg0 tail_apps

        | { fx = Infix RightAssoc; _ } ->
            (* sA_B(sA_B(sA_Bs)) -> Last token has to be of upper parsing level.  *)
            let* head_apps =
              iter1
                (let* arg0_i = prec_up in
                 let* args_i = between_parser in
                 return @@ Seq.cons arg0_i args_i)
            in
            let* argZ = prec_up in
            return @@ app_fold_right head_apps argZ
      in

      match operators with
      | [] -> Seq.empty
      | o :: os ->
          (operator_parser o || prec_lvl_parser prec_up os) @@ inp
    in
    match g with
    | [] -> app_parser inp
    | p :: ps ->
        let sucs = get_subgraph_parser ps in
        (prec_lvl_parser sucs (snd p) || sucs) @@ inp
  in
  get_subgraph_parser env.operators

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
