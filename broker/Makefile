.PHONY: deps

all: compile

compile:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps
	./rebar compile

run:
	erl -pa ebin/ deps/*/ebin -boot start_sasl -s main -config verboice
