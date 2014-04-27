PROJECT = mazurka_dsl

include erlang.mk

# noop
test: .eunit
	@touch erlang.mk

.eunit: ebin/*.beam test/mazurka_dsl_test.erl test/mazurka_dsl_lexer/*.mz test/mazurka_dsl_parser/*.mz
	@rebar eunit

.PHONY: eunit
