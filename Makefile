.PHONY: all compile deps eunit rel clean

all: compile

deps:
	@rebar get-deps

compile:
	@rebar compile

clean:
	@rebar clean

distclean: clean
	@rebar delete-deps

refresh: clean distclean deps compile

doc:
	@rebar skip_deps=true doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	   	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.bf_auth_dialyzer_plt

build_plt:
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps/*/ebin apps/*/ebin ebin

dialyzer:
	@echo
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) apps/*/ebin deps/*/ebin ebin

clean_plt:
	@echo
	@echo "Ary you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

ct:
	@rebar skip_deps=true ct

ct_all:
	@rebar ct

typer:
	typer --plt $(COMBO_PLT) -r apps -I deps -I apps

eunit:
	rm -f .eunit/*.dat
	@rebar skip_deps=true eunit

eunit_all:
	rm -f .eunit/*.dat
	@rebar eunit

test: all eunit ct

test_all: eunit_all ct_all
