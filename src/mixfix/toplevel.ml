(* Transcribing commands from Presyntax to Syntax  *)


let transcribe_cmd (env:Environment.t) (cmd: Presyntax.toplevel_cmd): Syntax.toplevel_cmd  =
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
     Syntax.Mixfix (prec, Operator.create assoc name)

  | Presyntax.Quit ->
     Syntax.Quit
  | Presyntax.Nop ->
     Syntax.Nop
  | Presyntax.GraphCmd g ->
    Syntax.GraphCmd (match g with
      | "print" -> Syntax.PrintGraph
      | "clear" -> Syntax.ClearGraph
      | _ -> Zoo.error ?kind:(Some "Command error") "Unknown :graph command: '%s'" g)

let transcribe_cmds (env:Environment.t) = 
  (* List.map (fun cmd -> toplevel_cmd env cmd) cmdlist *)
  let f (env', lst) pcmd = 
    match transcribe_cmd env' pcmd with
    | Syntax.Mixfix (prec, operator) -> (
        env' |> Environment.add_operator (prec, operator), lst
      )
    | Syntax.Def (x, _) as c -> (
      {env' with parser_context = env'.parser_context |> Environment.pctx_add_identifier x}, lst @ [c]
      )
    | c -> 
      (env', lst @ [c])
  in
  List.fold_left f (env, [])
