DEPS = deps/erlzmq deps/jiffy deps/gproc deps/ej \
       deps/chef_authn deps/sqerl deps/mixer deps\lager \
       deps/folsom

ERLANG_APPS = erts kernel stdlib tools compiler syntax_tools runtime_tools\
        crypto public_key ssl xmerl edoc eunit mnesia inets \
        mnesia hipe syntax_tools runtime_tools edoc eunit asn1 gs webtool \
        observer

PLT_FILE = .pushy.plt

all: dialyze eunit

use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = rebar -C $(rebar_config)

clean:
	$(REBAR) clean

allclean: depclean clean

depclean:
	@rm -rf deps

compile: $(DEPS)
	$(REBAR) compile

compile_app:
	$(REBAR) skip_deps=true compile

$(PLT_FILE):
	dialyzer -nn --output_plt $(PLT_FILE) --build_plt --apps $(ERLANG_APPS)

plt_deps:
	dialyzer --plt $(PLT_FILE) --output_plt $(PLT_FILE) --add_to_plt deps/*/ebin


dialyze: compile $(PLT_FILE)
	dialyzer -nn --plt $(PLT_FILE) -Wunmatched_returns -Werror_handling -Wrace_conditions -r apps/pushy/ebin -I deps

#dialyzer:
#	@rm -rf apps/pushy/.eunit
# Uncomment when stubbed functions in the FSM are complete
# @dialyzer -Wrace_conditions -Wunderspecs -r apps --src

$(DEPS):
	$(REBAR) get-deps

eunit: compile
	$(REBAR) eunit apps=pushy

eunit_app: compile_app
	$(REBAR) eunit apps=pushy skip_deps=true

test: eunit

tags:
	@find src deps -name "*.[he]rl" -print | etags -

rel: compile rel/opscode-pushy-server
rel/opscode-pushy-server:
	@cd rel;$(REBAR) generate overlay_vars=db_vars.config

relclean:
	@rm -rf rel/opscode-pushy-server

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach lib,$(wildcard apps/* deps/*), /bin/echo -n .;rm -rf rel/opscode-pushy-server/lib/$(shell basename $(lib))-* \
	   && ln -sf $(abspath $(lib)) rel/opscode-pushy-server/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

update: compile
	@cd rel/opscode-pushy-server;bin/opscode-pushy-server restart

update_app: compile_app
	@cd rel/opscode-pushy-server;bin/opscode-pushy-server restart

distclean: relclean
	@rm -rf deps
	$(REBAR) clean
