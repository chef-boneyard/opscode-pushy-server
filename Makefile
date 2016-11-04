REBAR3 = $(CURDIR)/rebar3
DOCKER_IMAGE_TAG = devchef/push-jobs-server

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

shell:
	if [ -z $$(docker images -q $(DOCKER_IMAGE_TAG)) ]; then docker build --tag $(DOCKER_IMAGE_TAG) .; fi; docker run --volume $(PWD):/root --interactive --tty $(DOCKER_IMAGE_TAG)
