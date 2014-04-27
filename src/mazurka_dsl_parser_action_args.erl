-module(mazurka_dsl_parser_action_args).

-export([parse/2]).

parse(Tokens, _Opts) ->
  scan(Tokens, []).

scan([], Acc) ->
  {ok, lists:reverse(Acc)};
scan([#{type := action_def_begin} = Action|Tokens], Acc) ->
  case combine(Action#{args => []}, Tokens) of
    {ok, CombinedActions, Rest} ->
      scan(Rest, [CombinedActions|Acc]);
    Error ->
      Error
  end;
scan([Token|Tokens], Acc) ->
  scan(Tokens, [Token|Acc]).

combine(#{args := Args} = Action, [#{type := action_def_body}|Tokens]) ->
  {ok, Action#{args := lists:reverse(Args), type := action}, Tokens};
combine(#{args := Args} = Action, [#{type := variable, value := Var}|Tokens]) ->
  combine(Action#{args := [Var|Args]}, Tokens);
combine(_, [Token|_]) ->
  {error, {unexpected, Token}}.
