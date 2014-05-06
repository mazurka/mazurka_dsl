PROJECT = mazurka_dsl

include erlang.mk

# noop
test: all eunit

eunit:
	@rebar eunit

.PHONY: eunit
