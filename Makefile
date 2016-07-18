# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

all: $(REBAR3)
	@$(REBAR3) do clean, compile

rel: all
	@$(REBAR3) release

test:
	@$(REBAR3) eunit ct

dialyzer:
	@$(REBAR3) dialyzer

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

update:
	$(REBAR3) update

install: $(REBAR3) distclean update

omnibus: $(REBAR3) install
	$(REBAR3) do compile, release

distclean:
	@rm -rf _build
