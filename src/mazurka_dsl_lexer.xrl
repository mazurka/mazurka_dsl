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
resource[\s]+@{Atom}+               :  {token, {def_begin,
                                                TokenLine,
                                                extract_resource(TokenChars),
                                                resource}}.

%%% attribute
([a-z]*)\s*::\s*(.*)                :  {token, split_attribute(TokenLine, TokenChars)}.

%% action

\)\s*->                             :  {token, {action_def_body,
                                                TokenLine}}.

\.                                  :  {token, {def_end,
                                                TokenLine}}.

%%% attribute
\-\s?                               :  {token, {attr_begin,
                                                TokenLine}}.

%% calls

%%% resource calls
@{Atom}\s*\(                        :  {token, {res_call_begin,
                                                TokenLine,
                                                parse_fn_call(TokenChars)}}.
@{Atom}:{Atom}\s*\(                 :  {token, {res_call_begin,
                                                TokenLine,
                                                parse_fn_call(TokenChars)}}.

%%% function calls
{Atom}\s*\(                         :  {token, {call_begin,
                                                TokenLine,
                                                parse_fn_call(TokenChars)}}.
{Atom}:{Atom}\s*\(                  :  {token, {call_begin,
                                                TokenLine,
                                                parse_fn_call(TokenChars)}}.
\)                                  :  {token, {call_end,
                                                TokenLine}}.

%%% variables
{Var}                               :  {token, {variable,
                                                TokenLine,
                                                unicode(TokenChars)}}.
%% datatypes

%%% string
\"[^\"]*\"                          :  {token, {string,
                                                TokenLine,
                                                parse_string(TokenChars)}}.
\'[^\']*\'                          :  {token, {string,
                                                TokenLine,
                                                parse_string(TokenChars)}}.

%%% numbers
{D}+                                :  {token, {integer,
                                                TokenLine,
                                                list_to_integer(TokenChars)}}.
{Float}                             :  {token, {float,
                                                TokenLine,
                                                list_to_float(TokenChars)}}.

%%% atom
{Atom}                              :  {token, {atom,
                                                TokenLine,
                                                list_to_atom(TokenChars)}}.

%%% maps, tuples, lists
#{                                  :  {token, {map_begin,
                                                TokenLine}}.
{                                   :  {token, {tuple_begin,
                                                TokenLine}}.
}                                   :  {token, {map_or_tuple_end,
                                                TokenLine}}.
\[                                  :  {token, {list_begin,
                                                TokenLine}}.
\]                                  :  {token, {list_end,
                                                TokenLine}}.

%% control
(\|\||\|)                           :  {token, {comp_sep,
                                                TokenLine}}.

%% assign
<-                                  :  {token, {assign,
                                                TokenLine}}.

%%% whitespace
{WS}                                :  skip_token.

Erlang code.

extract_resource(Str) ->
  <<"resource @", Name/binary>> = unicode(Str),
  binary_to_atom(Name).

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
      {token, {docstring, TokenLine, Block}};
    _ ->
      {error, "illegal nested block comment"}
  end.

parse_string(Str) ->
  Bin = unicode(Str),
  Len = byte_size(Bin) - 2,
  <<_, Content:Len/binary, _>> = Bin,
  Content.

parse_fn_call([$@|Str]) ->
  parse_fn_call(Str);
parse_fn_call(Str) ->
  Bin = unicode(Str),
  [Name|_] = binary:split(Bin, <<"(">>),
  case binary:split(Name, <<":">>) of
    [Mod,Fn] -> {binary_to_atom(Mod), binary_to_atom(Fn)};
    _ -> {'__global', binary_to_atom(Name)}
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

binary_to_atom(Bin) ->
  list_to_atom(binary_to_list(Bin)).
