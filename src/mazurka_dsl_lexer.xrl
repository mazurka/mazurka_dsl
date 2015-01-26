Definitions.

Char = [^\<\>\=\-\#\+\*\_\.\:\'\"\`\!\\\/\(\)\[\]\s\t\n\t]

Rules.

\=                                   :  {token, {'=',
                                                 TokenLine}}.

\-                                   :  {token, {'-',
                                                 TokenLine}}.

\#                                   :  {token, {'#',
                                                 TokenLine}}.

\<                                   :  {token, {'<',
                                                 TokenLine}}.

\>                                   :  {token, {'>',
                                                 TokenLine}}.

\+                                   :  {token, {'+',
                                                 TokenLine}}.

\*                                   :  {token, {'*',
                                                 TokenLine}}.

\_                                   :  {token, {'_',
                                                 TokenLine}}.

\.                                   :  {token, {'.',
                                                 TokenLine}}.

\:                                   :  {token, {':',
                                                 TokenLine}}.

\'                                   :  {token, {'\'',
                                                 TokenLine}}.

\"                                   :  {token, {'"',
                                                 TokenLine}}.

\`                                   :  {token, {'`',
                                                 TokenLine}}.

\~                                   :  {token, {'~',
                                                 TokenLine}}.

\!                                   :  {token, {'!',
                                                 TokenLine}}.

\\                                   :  {token, {'\\',
                                                 TokenLine}}.

\/                                   :  {token, {'/',
                                                 TokenLine}}.

\(                                   :  {token, {'(',
                                                 TokenLine}}.

\)                                   :  {token, {')',
                                                 TokenLine}}.

\[                                   :  {token, {'[',
                                                 TokenLine}}.

\]                                   :  {token, {']',
                                                 TokenLine}}.

\s                                   :  {token, {space,
                                                 TokenLine}}.

\t                                   :  {token, {tab,
                                                 TokenLine}}.

\n                                   :  {token, {newline,
                                                 TokenLine}}.

\n\r                                 :  {token, {newline,
                                                 TokenLine}}.

{Char}                               :  {token, {char,
                                                 TokenLine,
                                                 unicode(TokenChars)}}.

Erlang code.

unicode(Str) ->
  unicode:characters_to_binary(Str).
