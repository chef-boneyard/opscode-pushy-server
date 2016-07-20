REBAR3 = $(CURDIR)/rebar3

all:
	@$(REBAR3) do clean, compile

rel: all
	@$(REBAR3) release

test:
	@$(REBAR3) eunit

dialyzer:
	@$(REBAR3) dialyzer

update:
	@$(REBAR3) update

install: distclean update

omnibus: install
	@$(REBAR3) do compile, release

distclean:
	@rm -rf _build
