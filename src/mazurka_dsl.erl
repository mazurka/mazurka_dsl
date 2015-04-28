-module(mazurka_dsl).

-export([parse/2]).
-export([parse_file/2]).

parse(Src, Opts) ->
  case mazurka_dsl_lexer:string(Src) of
    {ok, Tokens, _} ->
      case mazurka_dsl_parser:parse(Tokens) of
        {ok, Ast} ->
          mazurka_dsl_compiler:compile(Ast, Opts);
        Error ->
          Error
      end;
    Error ->
      Error
  end.

parse_file(File, Opts) ->
  case read_file(File) of
    {ok, Src} ->
      parse(Src, Opts);
    Error ->
       Error
  end.

read_file(File) ->
  case file:read_file(File) of
    {ok, Bin} ->
      {ok, case unicode:characters_to_list(Bin) of
        L when is_list(L) ->
          L;
        _ ->
          binary_to_list(Bin)
      end};
     Error ->
       Error
  end.
