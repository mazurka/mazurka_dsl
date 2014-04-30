-module(mazurka_dsl).

-export([parse/2]).
-export([parse_file/2]).

parse(Src, _Opts) ->
  case mazurka_dsl_lexer:string(Src) of
    {ok, Tokens, _} ->
      {ok, Tokens2} = mazurka_dsl_parser_utils:insert_def_end(Tokens),
      mazurka_dsl_parser:parse(Tokens2);
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
