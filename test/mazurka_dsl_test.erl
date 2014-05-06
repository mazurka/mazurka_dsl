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

lex(Test) ->
  io:format("~p~n", [Test]),
  {ok, Bin} = file:read_file(Test ++ ".mz"),
  Src = decode(Bin),
  {ok, Out} = file:consult(Test ++ ".out"),

  {ok, Tokens, _} = mazurka_dsl_lexer:string(Src),
  Out =:= Tokens orelse ?debugFmt("~n~n  Actual:~n~n  ~p~n", [Tokens]),
  ?assertEqual(Out, Tokens).

%% parser_test_() ->
%%   {ok, CWD} = file:get_cwd(),
%%   Data = filename:join(CWD, "../test/mazurka_dsl_parser"),
%%   {ok, Tests} = file:list_dir(Data),
%%   [fun() -> parse(filename:join(Data, Test)) end || Test <- Tests].

%% parse(Test) ->
%%   {ok, Bin} = file:read_file(Test),
%%   Src = case unicode:characters_to_list(Bin) of
%%     L when is_list(L) ->
%%       L;
%%     _ ->
%%       binary_to_list(Bin)
%%   end,
%%   {ok, Tokens, _} = mazurka_dsl_lexer:string(Src),
%%   {ok, Tokens2} = mazurka_dsl_parser_utils:insert_def_end(Tokens),
%%   io:format("~p~n", [Tokens2]),

%%   {ok, AST} = mazurka_dsl_parser:parse(Tokens2),
%%   {ok, CWD} = file:get_cwd(),
%%   Out = filename:join(CWD, "../test.ast"),
%%   file:write_file(Out, io_lib:format("~p~n", [AST])).

decode(Bin) ->
  case unicode:characters_to_list(Bin) of
    L when is_list(L) ->
      L;
    _ ->
      binary_to_list(Bin)
  end.
