-module(mazurka_dsl_test).

-include_lib("eunit/include/eunit.hrl").

lexer_test_() ->
  {ok, CWD} = file:get_cwd(),
  Data = filename:join(CWD, "../test/mazurka_dsl_lexer"),
  {ok, Tests} = file:list_dir(Data),
  [fun() -> lex(filename:join(Data, Test)) end || Test <- Tests].

lex(Test) ->
  {ok, Bin} = file:read_file(Test),
  Src = case unicode:characters_to_list(Bin) of
    L when is_list(L) ->
      L;
    _ ->
      binary_to_list(Bin)
  end,
  %% TODO verify tokens
  {ok, _Tokens, _} = mazurka_dsl_lexer:string(Src),
  ?debugVal(_Tokens).

parser_test_() ->
  {ok, CWD} = file:get_cwd(),
  Data = filename:join(CWD, "../test/mazurka_dsl_parser"),
  {ok, Tests} = file:list_dir(Data),
  [fun() -> parse(filename:join(Data, Test)) end || Test <- Tests].

parse(Test) ->
  {ok, Bin} = file:read_file(Test),
  Src = case unicode:characters_to_list(Bin) of
    L when is_list(L) ->
      L;
    _ ->
      binary_to_list(Bin)
  end,
  {ok, Tokens, _} = mazurka_dsl_lexer:string(Src),
  {ok, AST} = mazurka_dsl_parser:parse(Tokens),
  ?debugVal(AST).
