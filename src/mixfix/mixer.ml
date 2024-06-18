(* Transcribing stuff from Presyntax to Syntax  *)

let determine_fixity assoc s =
    let first_char = String.get s 0 and last_char = String.get s (String.length s - 1) in
    match (first_char, last_char, assoc) with
    | ('_','_', Presyntax.LeftAssoc) -> Syntax.Infix Syntax.LeftAssoc
    | ('_','_', Presyntax.RightAssoc) -> Syntax.Infix Syntax.RightAssoc
    | ('_','_', Presyntax.NonAssoc) -> Syntax.Infix Syntax.NonAssoc
    | ('_',_, Presyntax.NonAssoc) -> Syntax.Postfix
    | (_, '_', Presyntax.NonAssoc) -> Syntax.Prefix
    | (_, _, Presyntax.NonAssoc) -> Syntax.Closed
    | _ -> 
      Zoo.error ?kind:(Some "Operator error") "Cannot provide associativity for postfix, prefix or closed operators. Use 'mixfix' instead." 

let create_operator assoc name =
  if not (String.contains_from name 0 '_') then
    Zoo.error ?kind:(Some "Operator error") "Operator must contain at least one underscore"
    else
      let tokens =  name |> String.split_on_char '_' |> List.filter (fun x -> x <> "") in
  let fx = determine_fixity assoc name in
    match tokens with 
    | [] -> Zoo.error ?kind:(Some "Operator error") "Empty operator"
    | tokens -> Syntax.{tokens ; fx }

let toplevel_cmd (env:Environment.t) (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
  match cmd with

  | Presyntax.Expr e ->
    print_endline @@ "Parsing " ^ Presyntax.string_of_expr e ;
    let e = Parser.(check_success @@ expr env.parser_context e) in
    Syntax.Expr e

  | Presyntax.Def (name, e) ->
    print_endline @@ "Parsing " ^ Presyntax.string_of_expr e ;
    let e = Parser.(check_success @@ expr env.parser_context e) in
    Syntax.Def (name, e)

  | Presyntax.Mixfix (assoc, prec, name) ->
     Syntax.Mixfix (prec, create_operator assoc name)

  | Presyntax.Quit ->
     Syntax.Quit
  | Presyntax.GraphCmd g ->
    Syntax.GraphCmd (match g with
      | "print" -> Syntax.PrintGraph
      | "clear" -> Syntax.ClearGraph
      | _ -> Zoo.error ?kind:(Some "Command error") "Unknown :graph command: '%s'" g)

let register_operator (prec, operator) (ctx:Environment.parser_context) =
  (* Check for duplicate tokens *)
  match Precedence.find_duplicate_token Syntax.(operator.tokens) ctx.operators with
  | Some (prec, token, op2) -> 
      Zoo.error ?kind:(Some "Operator error") "Duplicate token '%s' in operator %s. @." token (Syntax.string_of_op op2)
  | None ->
  (* End duplicate check *)
    let operators = Precedence.add_operator ctx.operators prec operator in
    {ctx with operators = operators}

let toplevel_cmds (env:Environment.t) cmdlist = 
  List.map (fun cmd -> toplevel_cmd env cmd) cmdlist
