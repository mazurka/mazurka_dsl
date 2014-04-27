-module(mazurka_dsl_parser_action_body).

-export([parse/2]).

parse(Tokens, _Opts) ->
  scan(Tokens, []).

scan([], Acc) ->
  {ok, lists:reverse(Acc)};
scan([#{type := action} = Action|Tokens], Acc) ->
  case combine(Action#{declarations => []}, Tokens) of
    {ok, CombinedActions, Rest} ->
      scan(Rest, [CombinedActions|Acc]);
    Error ->
      Error
  end;
scan([Token|Tokens], Acc) ->
  scan(Tokens, [Token|Acc]).

combine(#{declarations := Defs} = Res, []) ->
  {ok, Res#{declarations := lists:reverse(Defs)}, []};
combine(#{declarations := Defs} = Res, [#{type := action_def_end}|Rest]) ->
  {ok, Res#{declarations := lists:reverse(Defs)}, Rest};
combine(#{declarations := Defs} = Res, [Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens).
