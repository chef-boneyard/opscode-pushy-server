DEPS = deps/erlzmq deps/jiffy deps/gproc deps/ej deps/chef_common deps/sqerl deps/mixer

all: compile all_tests

all_tests: dialyzer eunit

use_locked_config = $(wildcard USE_REBAR_LOCKED)
ifeq ($(use_locked_config),USE_REBAR_LOCKED)
  rebar_config = rebar.config.lock
else
  rebar_config = rebar.config
endif
REBAR = rebar -C $(rebar_config)

#clean:
	#@rebar skip_deps=true clean

clean:
	@$(REBAR) clean

allclean: depclean clean

depclean:
	@rm -rf deps

compile: $(DEPS)
	@$(REBAR) compile

compile_app: $(DEPS)
	@$(REBAR) skip_deps=true compile

dialyzer:
	@rm -rf apps/pushy/.eunit
# Uncomment when stubbed functions in the FSM are complete
# @dialyzer -Wrace_conditions -Wunderspecs -r apps --src

$(DEPS):
	@$(REBAR) get-deps

eunit: compile
	@$(REBAR) eunit app=pushy

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

distclean: relclean
	@rm -rf deps
	@$(REBAR) clean
