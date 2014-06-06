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

APP_CMD =  erl -smp enable -pa ebin/ deps/*/ebin -boot start_sasl -s main -config verboice -sname verboice_broker@localhost

all: compile

compile: deps
	./rebar compile skip_deps=true

deps: rebar.config
	@if test -d deps; then \
		./rebar update-deps; \
	else \
		./rebar get-deps; \
	fi
	./rebar compile
	touch deps

eunit:
	./rebar eunit skip_deps=true

run:
	@if [ "$(RAILS_ENV)" = "production" ]; then \
		$(APP_CMD) -noinput +Bd; \
	else \
		$(APP_CMD) -s reloader; \
	fi

run-prod:
	$(APP_CMD) -noinput +Bd

remsh:
	erl -remsh verboice_broker@localhost -sname remsh +Bd

dialyzer: compile .apps.plt .deps.plt
	@dialyzer $(DIALYZER_OPTS) --plts .apps.plt .deps.plt -r ebin -Wno_undefined_callbacks --no_native --no_check_plt

.apps.plt:
	@dialyzer --build_plt --output_plt $@ --apps $(ERLANG_DIALYZER_APPS)

.deps.plt: deps/*/ebin/*.beam
	@dialyzer --build_plt --output_plt $@ deps/*/ebin

release:
	./rebar generate skip_deps=true
