Definitions.

B = [01]
O = [0-7]
D = [0-9]
U = [A-Z]
L = [a-z]
I = \n?(\s\s|\t)
WS = (\s|\t|\r|\n|,)
C = ({U}|{L})
Delim = [\s,]*

Atom = [a-z][0-9a-zA-Z_]*
Var = [A-Z_][0-9a-zA-Z_\/]*
Float = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?

Rules.

%%% docstring
#\|[^\|]*\|+([^#\|][^\|]*\|+)*#     :  block_comment(TokenLine, string:substr(TokenChars, 3)).

%%% comment
[;]+.*                              :  skip_token.

%% resource

%%% definition
resource[\s]+@{Atom}+               :  {token, {resource, TokenLine, extract_resource(TokenChars)}}.

%%% attribute
([a-z]*)\s*::\s*(.*)                :  {token, {resource_attribute, TokenLine, split_attribute(TokenChars)}}.

%% action

%%% definition
{Atom}\s*\(                         :  {token, {action_def_start, TokenLine, parse_action_def(TokenChars)}}.
{Var}                               :  {token, {variable, TokenLine, unicode(TokenChars)}}.
\)\s*->                             :  {token, {action_def_end, TokenLine}}.

%%% attribute
\-\-                                :  {token, {action_attribute, TokenLine}}.

%% calls

%%% resource calls
@{Atom}\s*\(                        :  {token, {start_res_call, TokenLine, parse_fn_call(TokenChars)}}.
@{Atom}:{Atom}\s*\(                 :  {token, {start_res_call, TokenLine, parse_fn_call(TokenChars)}}.

%%% function calls
{Atom}\s*\(                         :  {token, {start_fn_call, TokenLine, parse_fn_call(TokenChars)}}.
{Atom}:{Atom}\s*\(                  :  {token, {start_fn_call, TokenLine, parse_fn_call(TokenChars)}}.
\)                                  :  {token, {end_call, TokenLine}}.

%% datatypes

%%% string
\"[^\"]*\"                          :  {token, {string, TokenLine, parse_string(TokenChars)}}.
\'[^\']*\'                          :  {token, {string, TokenLine, parse_string(TokenChars)}}.

%%% numbers
{D}+                                :  {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Float}                             :  {token, {float, TokenLine, list_to_float(TokenChars)}}.

%%% atom
{Atom}                              :  {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

%%% maps, tuples, lists
#{                                  :  {token, {map_start, TokenLine}}.
{                                   :  {token, {tuple_start, TokenLine}}.
}                                   :  {token, {map_or_tuple_end}}.
\[                                  :  {token, {list_start, TokenLine}}.
\]                                  :  {token, {list_end, TokenLine}}.

%% control
\|\|                                :  {token, {comprehension, TokenLine}}.
\|                                  :  {token, {comprehension, TokenLine}}.

%% assignment
(=\>?|<-)                           :  {token, {assignment, TokenLine}}.

%%% whitespace
{I}                                 : {token, {indent, TokenLine}}.
{WS}                                : skip_token.

Erlang code.

extract_resource(Str) ->
  <<"resource @", Name/binary>> = unicode(Str),
  Name.

unicode(Str) ->
  unicode:characters_to_binary(Str).

split_attribute(Str) ->
  Bin = unicode(Str),
  [Key, Val] = binary:split(Bin, <<"::">>),
  {trim(Key), trim(Val)}.

block_comment(_TokenLine, "|#") ->
  skip_token;
block_comment(TokenLine, TokenChars) ->
  %% Check we're not opening another comment block.
  case string:str(TokenChars, "#|") of
    0 ->
      Bin = unicode(TokenChars),
      Length = byte_size(Bin) - 2,
      <<Block:Length/binary, "|#">> = Bin,
      {token, {docstring, TokenLine, trim(Block)}};
    _ ->
      {error, "illegal nested block comment"}
  end.

parse_string(Str) ->
  Bin = unicode(Str),
  Len = byte_size(Bin) - 2,
  <<_, Content:Len/binary, _>> = Bin,
  Content.

parse_action_def(Str) ->
  Bin = unicode(Str),
  [Name|_] = binary:split(Bin, <<"(">>),
  trim(Name).

parse_fn_call(Str) ->
  Bin = unicode(Str),
  [Name|_] = binary:split(Bin, <<"(">>),
  case binary:split(Name, <<":">>) of
    [Mod,Fn] -> {Mod, Fn};
    _ -> {'__local', Name}
  end.

trim(<<$\s, Rest/binary>>) ->
  trim(Rest);
trim(<<$\n, Rest/binary>>) ->
  trim(Rest);
trim(<<$\t, Rest/binary>>) ->
  trim(Rest);
trim(Bin) ->
  Len = byte_size(Bin) - 1,
  case Bin of
    <<First:Len/binary, $\s>> ->
      trim(First);
    <<First:Len/binary, $\n>> ->
      trim(First);
    <<First:Len/binary, $\t>> ->
      trim(First);
    _ ->
      Bin
  end.
