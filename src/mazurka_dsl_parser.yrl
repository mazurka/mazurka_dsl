Nonterminals

defs def def_

statements statement
assignment assignment_
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
affordance_begin
call_or_affordance_end
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

def -> def_ : '$1'.
def -> docstring def_ : set_doc('$2', '$1').
def -> attrs def_ : set_attrs('$2', '$1').
def -> docstring attrs def_ : set_attrs(set_doc('$3', '$1'), '$2').

def_ -> def_begin def_end : def_body('$1', []).
def_ -> def_begin statements def_end : def_body('$1', '$2').

%%% statements

statements -> statement : ['$1'].
statements -> statement statements : ['$1' | '$2'].

statement -> assignment : '$1'.
statement -> action : '$1'.

%%%% assignments

assignment -> assignment_ : '$1'.
assignment -> attrs assignment_ : set_attrs('$2', '$1').
assignment -> docstring assignment_ : set_doc('$2', '$1').
assignment -> docstring attrs assignment_ : set_attrs(set_doc('$3', '$1'), '$2').

assignment_ -> variable assign expr :
  #{
    type => assignment,
    value => ?value('$1'),
    children => #{
      0 => '$3'
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
      0 => '$3'
    }
  }.

%%% arguments

arguments -> variable : [to_map('$1', variable)].
arguments -> variable arguments : [to_map('$1', variable) | '$2'].

%%% exprs

exprs -> expr : ['$1'].
exprs -> expr exprs : ['$1' | '$2'].

expr -> expr_val : '$1'.
expr -> attrs expr_val :  set_attrs('$2', '$1').
expr -> docstring expr_val : set_doc('$2', '$1').
expr -> docstring attrs expr_val : set_attrs(set_doc('$3', '$1'), '$2').

expr_val -> integer : to_map('$1', literal).
expr_val -> float : to_map('$1', literal).
expr_val -> atom : to_map('$1', literal).
expr_val -> variable : to_map('$1', variable).
expr_val -> string : to_map('$1', literal).

%%% comprehensions

expr_val -> comprehension : '$1'.

comprehension ->
  list_begin expr comp_sep assignment list_end :
  #{
    type => comprehension,
    line => ?line('$1'),
    children => #{
      assignment => '$4',
      expression => '$2'
    }
  }.

%%% lists

expr_val ->
  list_begin list_end :
  #{
    type => list,
    line => ?line('$1'),
    children => #{}
  }.
expr_val ->
  list_begin exprs list_end :
  #{
    type => list,
    line => ?line('$1'),
    children => list_to_map('$2')
  }.

%%% tuples

expr_val ->
  tuple_begin map_or_tuple_end :
  #{
    type => tuple,
    line => ?line('$1'),
    children => #{}
  }.
expr_val ->
  tuple_begin exprs map_or_tuple_end :
  #{
    type => tuple,
    line => ?line('$1'),
    children => list_to_map('$2')
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
call -> call_begin exprs call_or_affordance_end : call_body('$1', '$2', call).
%%%% call with no arguments
call -> call_begin call_or_affordance_end : call_body('$1', [], call).
%%%% resource call with arguments
call -> affordance_begin exprs call_or_affordance_end : call_body('$1', '$2', affordance).
%%%% resource call with no arguments
call -> affordance_begin call_or_affordance_end : call_body('$1', [], affordance).

Erlang code.

%% Keep track of line info in tokens.
-define(line(Tup), element(2, Tup)).
-define(value(Tup), element(3, Tup)).

set_doc(Thing, Doc) ->
  Thing#{doc => ?value(Doc)}.

set_attrs(Thing, Attrs) ->
  Thing#{attrs => list_to_map(Attrs)}.

to_map({_, Line, Value}, Type) ->
  #{type => Type, line => Line, value => Value}.

def_body(Def, Statements) ->
  #{
    type => element(4, Def),
    value => ?value(Def),
    line => ?line(Def),
    children => list_to_map(Statements)
  }.

action_body(Action, Args, Statements) ->
  #{
    type => action,
    value => ?value(Action),
    line => ?line(Action),
    arguments => list_to_map(Args),
    children => list_to_map(Statements)
  }.

call_body(Call, Args, Type) ->
  #{
    type => Type,
    line => ?line(Call),
    value => ?value(Call),
    children => list_to_map(Args)
  }.

format_kvs([], Map) ->
  Map;
format_kvs([#{value := Key, expression := Expr}|KVs], Map) ->
  Map2 = maps:put(Key, Expr, Map),
  format_kvs(KVs, Map2).

list_to_map(List) ->
  list_to_map(lists:reverse(List), #{}).
list_to_map([], Acc) ->
  Acc;
list_to_map([V|Rest], Acc) ->
  Acc2 = maps:put(length(Rest), V, Acc),
  list_to_map(Rest, Acc2).
