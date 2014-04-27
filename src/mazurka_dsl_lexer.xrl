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
resource[\s]+@{Atom}+               :  {token, #{type => resource,
                                                 line => TokenLine,
                                                 value => extract_resource(TokenChars)}}.

%%% attribute
([a-z]*)\s*::\s*(.*)                :  {token, split_attribute(TokenLine, TokenChars)}.

%% action

%%% definition
{Atom}\s*\(                         :  {token, #{type => action_def_begin,
                                                 line => TokenLine,
                                                 value => parse_action_def(TokenChars)}}.
{Var}                               :  {token, #{type => variable,
                                                 line => TokenLine,
                                                 value => unicode(TokenChars)}}.
\)\s*->                             :  {token, #{type => action_def_body,
                                                 line => TokenLine}}.

\.                                  :  {token, #{type => action_def_end}}.

%%% attribute
\-\-                                :  {token, #{type => action_attribute,
                                                 line => TokenLine}}.

%% calls

%%% resource calls
@{Atom}\s*\(                        :  {token, #{type => call_begin,
                                                 subtype => resource,
                                                 line => TokenLine,
                                                 value => parse_fn_call(TokenChars)}}.
@{Atom}:{Atom}\s*\(                 :  {token, #{type => call_begin,
                                                 subtype => resource,
                                                 line => TokenLine,
                                                 value => parse_fn_call(TokenChars)}}.

%%% function calls
{Atom}\s*\(                         :  {token, #{type => call_begin,
                                                 subtype => resource,
                                                 line => TokenLine,
                                                 value => parse_fn_call(TokenChars)}}.
{Atom}:{Atom}\s*\(                  :  {token, #{type => call_begin,
                                                 subtype => resource,
                                                 line => TokenLine,
                                                 value => parse_fn_call(TokenChars)}}.
\)                                  :  {token, #{type => call_end,
                                                 line => TokenLine}}.

%% datatypes

%%% string
\"[^\"]*\"                          :  {token, #{type => string,
                                                 line => TokenLine,
                                                 value => parse_string(TokenChars)}}.
\'[^\']*\'                          :  {token, #{type => string,
                                                 line => TokenLine,
                                                 value => parse_string(TokenChars)}}.

%%% numbers
{D}+                                :  {token, #{type => integer,
                                                 line => TokenLine,
                                                 value => list_to_integer(TokenChars)}}.
{Float}                             :  {token, #{type => float,
                                                 line => TokenLine,
                                                 value => list_to_float(TokenChars)}}.

%%% atom
{Atom}                              :  {token, #{type => atom,
                                                 line => TokenLine,
                                                 value => list_to_atom(TokenChars)}}.

%%% maps, tuples, lists
#{                                  :  {token, #{type => map_begin,
                                                 line => TokenLine}}.
{                                   :  {token, #{type => tuple_begin,
                                                 line => TokenLine}}.
}                                   :  {token, #{type => map_or_tuple_end,
                                                 line => TokenLine}}.
\[                                  :  {token, #{type => list_begin,
                                                 line => TokenLine}}.
\]                                  :  {token, #{type => list_end,
                                                 line => TokenLine}}.

%% control
\|\|                                :  {token, #{type => comprehension,
                                                 line => TokenLine}}.
\|                                  :  {token, #{type => comprehension,
                                                 line => TokenLine}}.

%% assignment
(=\>?|<-)                           :  {token, #{type => assignment,
                                                 line => TokenLine}}.

%%% whitespace
{WS}                                : skip_token.

Erlang code.

extract_resource(Str) ->
  <<"resource @", Name/binary>> = unicode(Str),
  Name.

unicode(Str) ->
  unicode:characters_to_binary(Str).

split_attribute(TokenLine, Str) ->
  Bin = unicode(Str),
  [Key, Val] = binary:split(Bin, <<"::">>),
  #{type => resource_attribute,
    line => TokenLine,
    key => trim(Key),
    value => trim(Val)}.

block_comment(_TokenLine, "|#") ->
  skip_token;
block_comment(TokenLine, TokenChars) ->
  %% Check we're not opening another comment block.
  case string:str(TokenChars, "#|") of
    0 ->
      Bin = unicode(TokenChars),
      Length = byte_size(Bin) - 2,
      <<Block:Length/binary, "|#">> = Bin,
      {token, #{type => docstring, line => TokenLine, value => Block}};
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
