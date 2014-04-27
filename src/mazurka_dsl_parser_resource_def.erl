-module(mazurka_dsl_parser_resource_def).

-export([parse/2]).

parse(Tokens, _Opts) ->
  def(Tokens, []).

def([], Acc) ->
  {ok, lists:reverse(Acc)};
def([#{type := resource} = Res|Tokens], Acc) ->
  case combine(Res#{declarations => []}, Tokens) of
    {ok, Res2, Rest} ->
      def(Rest, [Res2|Acc]);
    Error ->
      Error
  end;
def([Token|Tokens], Acc) ->
  def(Tokens, [Token|Acc]).

combine(#{declarations := Defs} = Res, []) ->
  {ok, Res#{declarations := lists:reverse(Defs)}, []};
combine(#{declarations := Defs} = Res, [#{type := resource}|_] = Rest) ->
  {ok, Res#{declarations := lists:reverse(Defs)}, Rest};
combine(#{declarations := Defs} = Res, [Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens).
