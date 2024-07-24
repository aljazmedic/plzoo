module Mixfix = Zoo.Main(struct

  let name = "Mixfix"

  type command = Syntax.toplevel_cmd

  type environment = Environment.t

  let print_depth = ref 100

  let options = [("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]

  let initial_environment: environment = Environment.empty

  let file_parser = Some (fun environ s -> Toplevel.transcribe_cmds environ (Preparser.file Lexer.token s) )

  let toplevel_parser = Some (fun environ s -> Toplevel.transcribe_cmd environ (Preparser.toplevel Lexer.token s) )

  let exec (state:environment) = function
    | Syntax.Expr e ->
      (* type check [e], evaluate, and print result *)
       let ty = Type_check.type_of state.context e in
       let v = Interpret.interp state.env e in
       Zoo.print_info "- : %s = " (Presyntax.string_of_type ty) ;
       Interpret.print_result !print_depth v ;
       Zoo.print_info "@." ;
       {state with env = state.env}
       
    | Syntax.Def (x, e) ->
       (* type check [e], and store it unevaluated! *)
       let ty = Type_check.type_of state.context e in
       Zoo.print_info "val %s : %s@." x (Presyntax.string_of_type ty) ;
        {
          context = (x,ty)::state.context; 
          parser_context = state.parser_context |> 
            Environment.pctx_add_identifier x;
          env = (x, ref (Interpret.VClosure (state.env,e)))::state.env
        }
      | Syntax.Mixfix (prec, operator)-> 
       (* Add operator x with precedence prec and expression e to environment.operators *)
       state |> Environment.add_operator (prec, operator)
      | Syntax.Nop -> state
      | Syntax.Quit -> raise End_of_file
      | Syntax.OperatorsCmd gc ->
        match gc with 
        | Syntax.PrintOperators -> Zoo.print_info "%s\n" (Operator.string_of_graph state.parser_context.operators);
        state
        | Syntax.ClearOperators -> {state with parser_context = Environment.empty.parser_context}
end) ;;

Mixfix.main () ;;
