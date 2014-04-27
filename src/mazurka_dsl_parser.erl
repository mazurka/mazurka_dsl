-module(mazurka_dsl_parser).

-export([parse/1]).
-export([parse/2]).

-define(PASSES, [
  "set_filename",
  "docstring_merge",
  "fn_call",
  "map",
  "action_args",
  "action_body",
  "action_attr_merge",
  "resource_def"
]).

parse(Tokens) ->
  parse(Tokens, []).

parse(Tokens, Opts) ->
  parse(Tokens, Opts, ?PASSES).

parse(Ast, _, []) ->
  {ok, Ast};
parse(Tokens, Opts, [Pass|Passes]) ->
  Module = list_to_atom("mazurka_dsl_parser_" ++ Pass),
  case Module:parse(Tokens, Opts) of
    {ok, Ast} ->
      parse(Ast, Opts, Passes);
    Error ->
      Error
  end.
