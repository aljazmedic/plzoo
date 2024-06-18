(* Transcribing stuff from Presyntax to Syntax  *)

let determine_fixity assoc s =
    let first_char = String.get s 0 and last_char = String.get s (String.length s - 1) in
    match (first_char, last_char) with
    | ('_','_') -> (match assoc with
      | Presyntax.NonAssoc -> Syntax.Infix Syntax.NonAssoc
      | Presyntax.LeftAssoc -> Syntax.Infix Syntax.LeftAssoc
      | Presyntax.RightAssoc -> Syntax.Infix Syntax.RightAssoc)
    | ('_',_) -> Syntax.Postfix
    | (_, '_') -> Syntax.Prefix
    | (_, _) -> Syntax.Closed

let create_operator assoc name =
  let fx = determine_fixity assoc name in
    let tokens =  name |> String.split_on_char '_' |> List.filter (fun x -> x <> "") in
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
      | _ -> Zoo.error ?kind:(Some "Command error") "Unknown operator :graph command: '%s'" g)

let toplevel_cmds env = List.map @@ toplevel_cmd env
