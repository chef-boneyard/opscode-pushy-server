rel: compile test rel/opscode-pushy-server

rel/opscode-pushy-server:
	@cd rel
	$(REBAR) generate overlay_vars=db_vars.config

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

compile_app:
	$(REBAR) skip_deps=true compile
