Nonterminals

defs def def_

statements statement
assignments assignment assignment_
kvs kv kv_
action action_ action_body action_body_

attrs attr

exprs expr expr_val
arguments
call
comprehension
.

Terminals

%% defs
def_begin
def_end

%% statements
assign
attr_begin
action_def_body

%% exprs
call_begin
res_call_begin
call_end
integer
atom
float
string
list_begin
list_end
map_begin
tuple_begin
map_or_tuple_end
comp_sep

%% misc
docstring
variable
.

Rootsymbol defs.

%%% defs

defs -> def : ['$1'].
defs -> def defs : ['$1' | '$2'].

def -> docstring def_ : set_doc('$2', '$1').
def -> def_ : '$1'.

def_ -> def_begin def_end : def_body('$1', []).
def_ -> def_begin statements def_end : def_body('$1', '$2').

%%% statements

statements -> statement : ['$1'].
statements -> statement statements : ['$1' | '$2'].

statement -> assignment : '$1'.
statement -> action : '$1'.

%%%% assignments

assignments -> assignment : ['$1'].
assignments -> assignment assignments : ['$1' | '$2'].

assignment -> assignment_ : '$1'.
assignment -> attrs assignment_ : set_attrs('$2', '$1').
assignment -> docstring assignment_ : set_doc('$2', '$1').
assignment -> docstring attrs assignment_ : set_attrs(set_doc('$3', '$1'), '$2').

assignment_ -> variable assign expr :
  #{
    type => assignment,
    value => ?value('$1'),
    children => #{
      expressions => ['$3']
    }
  }.

%%%% actions

action -> action_ : '$1'.
action -> attrs action_ : set_attrs('$2', '$1').
action -> docstring action_ : set_doc('$2', '$1').
action -> docstring attrs action_ : set_attrs(set_doc('$3', '$1'), '$2').

action_ -> call_begin action_def_body action_body def_end : action_body('$1', [], '$3').
action_ -> call_begin arguments action_def_body action_body def_end : action_body('$1', '$2', '$4').

action_body -> action_body_ : ['$1'].
action_body -> action_body_ action_body : ['$1' | '$2'].

action_body_ -> expr : '$1'.
action_body_ -> assignment : '$1'.

%%% attributes

attrs -> attr : ['$1'].
attrs -> attr attrs : ['$1' | '$2'].

attr ->
  attr_begin atom def_end :
  #{
    type => attr,
    value => ?value('$2')
  }.

attr ->
  attr_begin atom expr def_end :
  #{
    type => attr,
    value => ?value('$2'),
    children => #{
      expressions => ['$3']
    }
  }.

%%% arguments

arguments -> variable : [to_map('$1')].
arguments -> variable arguments : [to_map('$1') | '$2'].

%%% exprs

exprs -> expr : ['$1'].
exprs -> expr exprs : ['$1' | '$2'].

expr -> expr_val : '$1'.
expr -> attrs expr_val :  set_attrs('$2', '$1').
expr -> docstring expr_val : set_doc('$2', '$1').
expr -> docstring attrs expr_val : set_attrs(set_doc('$3', '$1'), '$2').

expr_val -> integer : to_map('$1').
expr_val -> float : to_map('$1').
expr_val -> atom : to_map('$1').
expr_val -> variable : to_map('$1').
expr_val -> string : to_map('$1').

%%% comprehensions

expr_val -> comprehension : '$1'.

comprehension ->
  list_begin action_body comp_sep assignment list_end :
  #{
    type => comprehension,
    line => ?line('$1'),
    children => #{
      assignments => ['$4'],
      expressions => '$2'
    }
  }.

comprehension ->
  list_begin action_body comp_sep assignment exprs list_end :
  #{
    type => comprehension,
    line => ?line('$1'),
    children => #{
      assignments => ['$4'],
      expressions => '$2',
      filters => '$5'
    }
  }.

%%% lists

expr_val ->
  list_begin list_end :
  #{
    type => list,
    line => ?line('$1'),
    children => #{
      values => []
    }
  }.
expr_val ->
  list_begin exprs list_end :
  #{
    type => list,
    line => ?line('$1'),
    children => #{
      values => '$2'
    }
  }.

%%% tuples

expr_val ->
  tuple_begin map_or_tuple_end :
  #{
    type => tuple,
    line => ?line('$1'),
    children => #{
      values => []
    }
  }.
expr_val ->
  tuple_begin exprs map_or_tuple_end :
  #{
    type => tuple,
    line => ?line('$1'),
    children => #{
      values => '$2'
    }
  }.

%%% maps

expr_val ->
  map_begin map_or_tuple_end :
  #{
    type => map,
    line => ?line('$1'),
    children => #{}
  }.
expr_val ->
  map_begin kvs map_or_tuple_end :
  #{
    type => map,
    line => ?line('$1'),
    children => format_kvs('$2', #{})
  }.

%%%% KVs

kvs -> kv : ['$1'].
kvs -> kv kvs : ['$1' | '$2'].

kv -> kv_ : '$1'.
kv -> attrs kv_ : set_attrs('$2', '$1').
kv -> docstring kv_ : set_doc('$2', '$1').
kv -> docstring attrs kv_ : set_attrs(set_doc('$3', '$1'), '$2').

kv_ ->
  atom assign expr :
  #{
    type => kv,
    value => ?value('$1'),
    expression => '$3'
  }.

%%% calls

expr_val -> call : '$1'.

%%%% call with arguments
call -> call_begin exprs call_end : call_body('$1', '$2', fn).
%%%% call with no arguments
call -> call_begin call_end : call_body('$1', [], fn).
%%%% resource call with arguments
call -> res_call_begin exprs call_end : call_body('$1', '$2', resource).
%%%% resource call with no arguments
call -> res_call_begin call_end : call_body('$1', [], resource).

Erlang code.

%% Keep track of line info in tokens.
-define(line(Tup), element(2, Tup)).
-define(value(Tup), element(3, Tup)).

set_doc(Thing, Doc) ->
  Thing#{doc => ?value(Doc)}.

set_attrs(Thing, Attrs) ->
  Thing#{attrs => Attrs}.

to_map({Type, Line, Value}) ->
  #{type => Type, line => Line, value => Value}.

def_body(Def, Statements) ->
  #{
    type => element(4, Def),
    value => ?value(Def),
    children => #{
      statements => Statements
    }
  }.

action_body(Action, Args, Statements) ->
  {_, Name} = ?value(Action),
  #{
    type => action,
    value => Name,
    children => #{
      statements => Statements,
      arguments => Args
    }
  }.

call_body(Call, Args, Type) ->
  #{type => call,
    subtype => Type,
    line => ?line(Call),
    value => ?value(Call),
    declarations => #{
      arguments => Args
    }}.

format_kvs([], Map) ->
  Map;
format_kvs([#{value := Key, expression := Expr}|KVs], Map) ->
  Map2 = maps:put(Key, [Expr], Map),
  format_kvs(KVs, Map2).
