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

  | Presyntax.Def (name,  e) ->
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

let toplevel_cmds env = List.map @@ toplevel_cmd env
