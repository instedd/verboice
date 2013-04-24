.PHONY: deps

ERLANG_DIALYZER_APPS = asn1 \
                     compiler \
                     crypto \
                     edoc \
                     erts \
                     eunit \
                     gs \
                     hipe \
                     inets \
                     kernel \
                     mnesia \
                     observer \
                     public_key \
                     runtime_tools \
                     ssl \
                     stdlib \
                     syntax_tools \
                     tools \
                     webtool \
                     xmerl

all: compile

compile:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps
	./rebar compile

run:
	erl -pa ebin/ deps/*/ebin -boot start_sasl -s main -config verboice

dialyzer: compile .apps.plt .deps.plt
	@dialyzer $(DIALYZER_OPTS) --plts .apps.plt .deps.plt -r ebin -Wno_undefined_callbacks --no_native --no_check_plt

.apps.plt:
	@dialyzer --build_plt --output_plt $@ --apps $(ERLANG_DIALYZER_APPS)

.deps.plt: deps/*/ebin/*.beam
	@dialyzer --build_plt --output_plt $@ deps/*/ebin
