-module(mazurka_dsl_parser_docstring_merge).

-export([parse/2]).

parse(Tokens, _Opts) ->
  merge(Tokens, []).

merge([], Acc) ->
  {ok, lists:reverse(Acc)};
merge([#{type := docstring, value := Val}, Next|Tokens], Acc) ->
  merge(Tokens, [Next#{docstring => Val}|Acc]);
merge([Token|Tokens], Acc) ->
  merge(Tokens, [Token|Acc]).
