-module(mazurka_dsl_parser).

-export([parse/1]).

-define(line(Tup), element(2, Tup)).
-define(value(Tup), element(3, Tup)).

parse(Tokens) ->
  parse(Tokens, []).

%% TODO trim the first three spaces

parse([], Acc) ->
  {ok, lists:reverse(Acc, [])};
parse([{'#', Line} | Rest], Acc) ->
  {Header, Rest2} = atx_header(Rest, 1, Line),
  parse(Rest2, [Header | Acc]);
parse([{'=', Line} | Rest], [Text | Acc]) ->
  {H, Rest2} = setext_header(Rest, Text, <<"h1">>, Line),
  parse(Rest2, [H | Acc]);
parse([{'-', Line} | Rest], [Text | Acc]) ->
  {H, Rest2} = setext_header(Rest, Text, <<"h2">>, Line),
  parse(Rest2, [H | Acc]);
parse([{'`', Line}, {'`', Line}, {'`', Line} | Rest], Acc) ->
  {Code, Rest2} = fenced_code(Rest, Line, '`'),
  parse(Rest2, [Code | Acc]);
parse([{'~', Line}, {'~', Line}, {'~', Line} | Rest], Acc) ->
  {Code, Rest2} = fenced_code(Rest, Line, '~'),
  parse(Rest2, [Code | Acc]);
parse(Rest, Acc) ->
  {Line, Rest2} = line(Rest),
  P = #{type => tag,
        name => <<"p">>,
        children => Line},
  parse(Rest2, [P | Acc]).

%% flatten_acc([], Acc) ->
%%   Acc;
%% flatten_acc([Item | Rest], Acc) when is_list(Item) ->
%%   flatten_acc(Rest, Item ++ Acc);
%% flatten_acc([Item | Rest], Acc) ->
%%   flatten_acc(Rest, [Item | Acc]).

link(Rest, LineNo) ->
  {Props, Children, Rest2} = parse_link(Rest, [], [], #{}, ']', LineNo),
  Link = #{type => tag,
           name => <<"a">>,
           children => Children,
           props => Props,
           line => LineNo},
  {Link, Rest2}.

parse_link([{']', _}, {'(', _} | Rest], Acc, _, Props, ']', Line) ->
  Child = acc_to_binary(Acc, Line),
  parse_link(Rest, [], [Child], Props, ')', Line);
parse_link([{')', _} | Rest], Acc, Children, Props, ')', Line) ->
  #{value := Href} = acc_to_binary(Acc, Line),
  {Props#{href => Href}, Children, Rest};
parse_link([{Token, _} | Rest], Acc, Children, Props, Char, Line) ->
  parse_link(Rest, [token_char(Token) | Acc], Children, Props, Char, Line);
parse_link([Item|Rest], Acc, Children, Props, Char, Line) ->
  parse_link(Rest, [?value(Item) | Acc], Children, Props, Char, Line).

%% TODO make sure the line only contains the character
setext_header(Rest, Text, Name, Line) ->
  {_, Rest2} = line(Rest),
  H = #{type => tag,
        name => Name,
        children => Text,
        line => Line},
  {H, Rest2}.

%% TODO limit header size to 6
%% TODO require a space between
%% TODO allow 1-3 spaces before #
%% TODO trim the trailing #
atx_header([{'#', _} | Rest], Weight, Line) ->
  atx_header(Rest, Weight + 1, Line);
atx_header(Rest, Weight, LineNo) ->
  {Line, Rest2} = line(trim(Rest)),
  H = #{type => tag,
        name => <<"h", (integer_to_binary(Weight))/binary>>,
        children => Line,
        line => LineNo},
  {H, Rest2}.

fenced_code(Rest, LineNo, Char) ->
  {[#{type := text, value := Class}], Rest2} = line(trim(Rest)),
  {Children, Rest3} = fenced_code_body(Rest2, [], Char),
  H = #{type => tag,
        name => <<"code">>,
        children => Children,
        info => Class,
        line => LineNo},
  {H, Rest3}.

fenced_code_body([], _, _) ->
  throw({error, missing_fenced_code_close});
fenced_code_body([{'`', Line}, {'`', Line}, {'`', Line} | Rest], Acc, '`') ->
  {_, Rest2} = line(Rest),
  {lists:reverse(Acc), Rest2};
fenced_code_body([{'~', Line}, {'~', Line}, {'~', Line} | Rest], Acc, '~') ->
  {_, Rest2} = line(Rest),
  {lists:reverse(Acc), Rest2};
fenced_code_body(Rest, Acc, Char) ->
  {[Line], Rest2} = line(Rest, noparse),
  fenced_code_body(Rest2, [Line | Acc], Char).

trim([{space, _} | Rest]) ->
  trim(Rest);
trim(Rest) ->
  Rest.

line(List) ->
  line(List, parse).
line(List, Mode) ->
  line(List, [], [], Mode).

line([], Acc, Parts, _) ->
  %% TODO fix the 0 line
  {lists:reverse([acc_to_binary(Acc, 0)|Parts]), []};
line([{newline, Line} | Rest], Acc, Parts, _) ->
  {lists:reverse([acc_to_binary(Acc, Line)|Parts]), Rest};
%% TODO match on inline tags, code, etc
line([{'[', Line} | Rest], Acc, Parts, parse) ->
  {Link, Rest2} = link(Rest, Line),
  case acc_to_binary(Acc, Line) of
    <<>> ->
      line(Rest2, [], [Link | Parts], parse);
    Prev ->
      line(Rest2, [], [Link, Prev | Parts], parse)
  end;
line([{Token, _}|Rest], Acc, Parts, Mode) ->
  line(Rest, [token_char(Token) | Acc], Parts, Mode);
line([Item|Rest], Acc, Parts, Mode) ->
  line(Rest, [?value(Item) | Acc], Parts, Mode).

acc_to_binary(Acc, Line) ->
  #{type => text,
    value => list_to_binary(lists:reverse(Acc)),
    line => Line}.

token_char(newline) -> $\n;
token_char(tab) -> $\t;
token_char(space) -> $\s;
token_char('=') -> $=;
token_char('-') -> $-;
token_char('#') -> $#;
token_char('<') -> $<;
token_char('>') -> $>;
token_char('+') -> $+;
token_char('*') -> $*;
token_char('_') -> $_;
token_char('.') -> $.;
token_char(':') -> $:;
token_char('\'') -> $\';
token_char('"') -> $\";
token_char('~') -> $~;
token_char('`') -> $`;
token_char('!') -> $!;
token_char('\\') -> $\\;
token_char('/') -> $/;
token_char('(') -> $(;
token_char(')') -> $);
token_char('[') -> $[;
token_char(']') -> $].
