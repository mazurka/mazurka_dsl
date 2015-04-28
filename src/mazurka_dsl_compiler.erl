-module(mazurka_dsl_compiler).

-export([compile/2]).

compile(AST, _Opts) ->
  scan_resource(AST, []).

scan_resource([], Acc) ->
  {ok, lists:reverse(Acc)};
scan_resource([#{name := <<"h1">>, children := NameList, line := Line} = _RouteTag | Rest], Acc) ->
  Name = format_text(NameList, []),
  {Docs, Rest2} = scan_html(Rest, []),
  {MediaTypes, Rest3} = scan_mediatypes(Rest2, []),
  Route = #{type => resource,
            name => to_atom(Name),
            docs => Docs,
            children => MediaTypes,
            line => Line},
  scan_resource(Rest3, [Route | Acc]).

scan_html([#{name := <<"p">>, children := Children} | Rest], Acc) ->
  case format_text(Children, []) of
    <<>> when Acc =:= [] ->
      scan_html(Rest, Acc);
    Out ->
      scan_html(Rest, [$\n, Out | Acc])
  end;
scan_html(Rest, []) ->
  {<<>>, Rest};
scan_html(Rest, [$\n | Acc]) ->
  {list_to_binary(lists:reverse(Acc)), Rest}.

format_text(Tokens, Acc) ->
  format_text(Tokens, Acc, []).

format_text([], Acc, _) ->
  list_to_binary(lists:reverse(Acc));
format_text([#{type := text, value := Text} | Rest], Acc, Separator) ->
  format_text(Rest, [Separator, Text | Acc], Separator).

scan_mediatypes([], Acc) ->
  {lists:reverse(Acc), []};
scan_mediatypes([#{name := <<"h2">>, children := NameList, line := Line} | Rest], Acc) ->
  Name = format_text(NameList, []),
  {Parser, Rest2} = scan_parser(Rest),
  {Sections, Rest3} = scan_sections(Rest2, [], Parser),
  MediaType = #{type => mediatype,
                types => re:split(Name, <<", *">>),
                children => Sections,
                line => Line},
  scan_mediatypes(Rest3, [MediaType | Acc]);
scan_mediatypes([#{name := <<"h1">>} | _] = Rest, Acc) ->
  {lists:reverse(Acc), Rest}.

scan_sections([], Acc, _) ->
  {lists:reverse(Acc), []};
scan_sections([#{name := <<"h3">>, children := NameList} | Rest], Acc, DefaultParser) ->
  Name = format_text(NameList, []),
  {Docs, Rest2} = scan_html(Rest, []),
  {Parser, Code, Line, Rest3} = scan_code(Rest2, DefaultParser),
  Section = #{type => section,
              name => to_atom(Name),
              parser => Parser,
              docs => Docs,
              code => Code,
              line => Line},
  scan_sections(Rest3, [Section | Acc], DefaultParser);
scan_sections([#{name := Name} | _] = Rest, Acc, _) when Name =:= <<"h1">> orelse Name =:= <<"h2">> ->
  {lists:reverse(Acc), Rest};
scan_sections([_ | Rest], Acc, DefaultParser) ->
  scan_sections(Rest, Acc, DefaultParser).

scan_code([#{name := <<"code">>, children := CodeList, info := Parser, line := Line} | Rest], DefaultParser) ->
  Code = format_text(CodeList, [], $\n),
  Chosen = case Parser of
    <<>> -> DefaultParser;
    _ -> Parser
  end,
  {Chosen, Code, Line, Rest}.

scan_parser([#{name := <<"p">>, children := [_, #{name := <<"a">>, children := NameList, props := Props}, _]} | Rest]) ->
  case format_text(NameList, []) of
    <<"parser">> ->
      {maps:get(href, Props), Rest};
    Name ->
      throw({invalid_link, Name})
  end;
scan_parser([_ | Rest]) ->
  scan_parser(Rest).

to_atom(Bin) ->
  list_to_atom(binary_to_list(Bin)).
