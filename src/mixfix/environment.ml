
module IdentifierSet = Set.Make(struct type t = Syntax.name let compare = Stdlib.compare end)

type parser_context = {
  operators: Operator.graph;
  known_identifiers: IdentifierSet.t;
}

type t = {
  parser_context: parser_context;
  context: (Syntax.name * Presyntax.htype) list;
  env: Interpret.environment;
}
;;

let empty = {
  parser_context = {
    operators = Operator.empty_graph;
    known_identifiers = IdentifierSet.empty;
  };
  context = [];
  env = [];
}
;;

let debug = true

let pctx_add_identifier identifier (pctx:parser_context) =
  {pctx with known_identifiers = IdentifierSet.add identifier pctx.known_identifiers}

let add_operator (prec, operator) (ctx:t) =
  {ctx with parser_context = {
    ctx.parser_context with operators = (Operator.add_operator (prec, operator) ctx.parser_context.operators)
  }}
  
let identifier_present (ctx:parser_context) token =
  IdentifierSet.mem token ctx.known_identifiers

let dprintln a =
  if debug then print_endline a else ()