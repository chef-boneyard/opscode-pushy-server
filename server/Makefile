DEPS = deps/erlzmq deps/jiffy

all: compile all_tests

all_tests: dialyzer eunit

clean:
	@rebar skip_deps=true clean

allclean: depclean clean

depclean:
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

dialyzer:
	rm -rf apps/pushy/.eunit
	@dialyzer -Wrace_conditions -Wunderspecs -r apps --src

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar eunit app=pushy

test: eunit

tags:
	find src deps -name "*.[he]rl" -print | etags -

rel: rel/pushy
rel/pushy:
	cd rel;rebar generate overlay_vars=db_vars.config

relclean:
	rm -rf rel/pushy

devrel: rel
	@/bin/echo -n Symlinking deps and apps into release
	@$(foreach dep,$(wildcard deps/*), /bin/echo -n .;rm -rf rel/erchef/lib/$(shell basename $(dep))-* \
	   && ln -sf $(abspath $(dep)) rel/erchef/lib;)
	@/bin/echo done.
	@/bin/echo  Run \'make update\' to pick up changes in a running VM.

update: compile
	@cd rel/pushy;bin/pushy restart
