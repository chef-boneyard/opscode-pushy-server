DEPS = deps/erlzmq deps/jiffy deps/gproc deps/ej \
       deps/chef_authn deps/sqerl deps/mixer deps\lager \
       deps/folsom

all: compile all_tests

all_tests: dialyze eunit

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

.pushy.plt:
	dialyzer -nn --output_plt .pushy.plt --build_plt --apps erts kernel stdlib crypto public_key

dialyze: .pushy.plt
	dialyzer -nn --plt .pushy.plt --src -Wunmatched_returns -Werror_handling -Wrace_conditions -r apps/pushy/src -I deps

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

rel: compile rel/pushy
rel/pushy:
	@cd rel;$(REBAR) generate overlay_vars=db_vars.config

relclean:
	@rm -rf rel/pushy

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/pushy/lib/$(shell basename $(dep))-* \
	   && ln -sf $(abspath $(dep)) rel/pushy/lib;)
	@rm -rf rel/pushy/lib/pushy*;ln -sf $(abspath apps/pushy) rel/pushy/lib/pushy
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

update: compile
	@cd rel/pushy;bin/pushy restart

update_app: compile_app
	@cd rel/pushy;bin/pushy restart

distclean: relclean
	@rm -rf deps
	$(REBAR) clean
