-module(mazurka_dsl_parser_action_args).

-export([parse/2]).

parse(Tokens, _Opts) ->
  scan(Tokens, []).

scan([], Acc) ->
  {ok, lists:reverse(Acc)};
scan([#{type := call_begin} = Action|Tokens], Acc) ->
  case combine(Action#{args => []}, Tokens) of
    {ok, CombinedActions, Rest} ->
      scan(Rest, [CombinedActions|Acc]);
    pass ->
      scan(Tokens, [Action|Acc]);
    Error ->
      Error
  end;
scan([Token|Tokens], Acc) ->
  scan(Tokens, [Token|Acc]).

combine(#{args := Args, value := {_, Name}} = Action, [#{type := action_def_body}|Tokens]) ->
  {ok, Action#{args := lists:reverse(Args), type := action, value := Name}, Tokens};
combine(#{args := Args} = Action, [#{type := variable, value := Var}|Tokens]) ->
  combine(Action#{args := [Var|Args]}, Tokens);
combine(_, [_|_]) ->
  pass.
