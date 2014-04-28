-module(mazurka_dsl_parser_utils).

-export([insert_def_end/1]).

insert_def_end(Tokens) ->
  insert_def_end(Tokens, false, []).

insert_def_end([], _, [Last|_] = Acc) ->
  Line = element(2, Last),
  {ok, lists:reverse([{def_end, Line}|Acc])};
insert_def_end([{def_begin, _, _, _} = Token|Tokens], false, Acc) ->
  insert_def_end(Tokens, true, [Token|Acc]);
insert_def_end([{def_begin, _, _, _} = Token|Tokens], true, Acc) ->
  Line = element(2, Token),
  insert_def_end(Tokens, true, [Token,{def_end, Line}|Acc]);
insert_def_end([{docstring, _, _} = Doc, {def_begin, _, _, _} = Token|Tokens], true, Acc) ->
  Line = element(2, Token),
  insert_def_end(Tokens, true, [Token,Doc,{def_end, Line}|Acc]);
insert_def_end([Token|Tokens], Found, Acc) ->
  insert_def_end(Tokens, Found, [Token|Acc]).
