DEPS ?= $(CURDIR)/deps

DIALYZER_OPTS ?= -Wunderspecs

DIALYZER_DIRS ?= ebin

DEPS_PLT = deps.plt

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

all: .concrete/DEV_MODE compile eunit dialyzer $(ALL_HOOK)

.concrete/DEV_MODE:
	@mkdir -p .concrete
	@touch $@

# Clean ebin and .eunit of this project
clean:
	@$(REBAR) clean skip_deps=true

# Clean this project and all deps
allclean:
	@$(REBAR) clean

compile: $(DEPS)
	@$(REBAR) compile

$(DEPS):
	@$(REBAR) get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@$(REBAR) clean

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit

dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r $(DIALYZER_DIRS)

$(DEPS_PLT):
	dialyzer --build_plt $(shell ./dialyzer_deps.sh $(DIALYZER_SKIP_DIRS)) --output_plt $(DEPS_PLT)

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

doc:
	@$(REBAR) doc skip_deps=true

tags:
	find src deps -name "*.[he]rl" -print | etags -

.PHONY: all compile eunit test dialyzer clean allclean distclean doc tags
