-module(mazurka_dsl_parser_fn_call).

-export([parse/2]).

parse(Tokens, _Opts) ->
  def(Tokens, []).

def([], Acc) ->
  {ok, lists:reverse(Acc)};
def([#{type := call_begin} = Res|Tokens], Acc) ->
  case declare(Res, Tokens) of
    {ok, Res2, Rest} ->
      def(Rest, [Res2|Acc]);
    Error ->
      Error
  end;
def([Token|Tokens], Acc) ->
  def(Tokens, [Token|Acc]).

declare(#{declarations := _Defs} = Res, Tokens) ->
  case combine(Res, Tokens) of
    {ok, Res2, Rest} ->
      {ok, Res2#{type := call}, Rest};
    Error ->
      Error
  end;
declare(Res, Tokens) ->
  declare(Res#{declarations => []}, Tokens).

combine(_, []) ->
  {error, {unexpected, 'EOF'}};
combine(#{declarations := Defs} = Res, [#{type := call_end}|_] = Rest) ->
  {ok, Res#{declarations := lists:reverse(Defs)}, Rest};
combine(#{declarations := Defs} = Res, [#{type := call_begin} = Begin|Tokens]) ->
  case declare(Begin, Tokens) of
    {ok, Call, Rest} ->
      combine(Res#{declarations := [Call|Defs]}, Rest);
    Error ->
      Error
  end;
combine(#{declarations := Defs} = Res, [#{type := integer} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := string} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := float} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := atom} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := list} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := map} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(#{declarations := Defs} = Res, [#{type := variable} = Token|Tokens]) ->
  combine(Res#{declarations := [Token|Defs]}, Tokens);
combine(_, [Token|_]) ->
  {error, {unexpected, Token}}.
