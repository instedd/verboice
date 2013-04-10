all: compile

compile:
	./rebar compile skip_deps=true

run:
	erl -pa ebin/ deps/*/ebin -boot start_sasl -s main -config verboice
