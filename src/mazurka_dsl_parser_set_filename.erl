-module(mazurka_dsl_parser_set_filename).

-export([parse/2]).

parse(Tokens, #{filename := Filename}) ->
  set(Tokens, Filename, []);
parse(Tokens, _Opts) ->
  {ok, Tokens}.

set([], _, Acc) ->
  {ok, lists:reverse(Acc)};
set([Token|Tokens], Filename, Acc) ->
  set(Tokens, Filename, [Token#{filename => Filename}|Acc]).
