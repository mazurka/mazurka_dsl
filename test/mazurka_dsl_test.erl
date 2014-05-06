-module(mazurka_dsl_test).

-include_lib("eunit/include/eunit.hrl").

-define(WILDCARD(Type), (begin
  {ok, CWD} = file:get_cwd(),
  Folder = case filename:basename(CWD) of
    ".eunit" -> filename:join(CWD, "../test/" ++ Type);
    _ -> filename:join(CWD, "test/" ++ Type)
  end,
  Tests = filelib:wildcard(Folder ++ "/*.mz"),
  [filename:join(Folder, filename:basename(Test, ".mz")) || Test <- Tests]
end)).

lexer_test_() ->
  [fun() -> lex(Test) end || Test <- ?WILDCARD("lexer")].

parser_test_() ->
  [fun() -> parse(Test) end || Test <- ?WILDCARD("parser")].

lex(Test) ->
  {ok, Bin} = file:read_file(Test ++ ".mz"),
  Src = decode(Bin),
  {ok, Out} = file:consult(Test ++ ".out"),

  {ok, Tokens, _} = mazurka_dsl_lexer:string(Src),
  Out =:= Tokens orelse ?debugFmt("~n~n  Actual:~n~n  ~p~n", [Tokens]),
  ?assertEqual(Out, Tokens),
  Tokens.

parse(Test) ->
  %% TODO run through a lint
  {ok, _Res} = mazurka_dsl:parse_file(Test ++ ".mz", []).

decode(Bin) ->
  case unicode:characters_to_list(Bin) of
    L when is_list(L) ->
      L;
    _ ->
      binary_to_list(Bin)
  end.
