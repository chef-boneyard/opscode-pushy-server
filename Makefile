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

update-image:
	@docker build --tag $(DOCKER_IMAGE_TAG) .

# depend directly on update-image - it makes it simple
# to tweak the image, and doesn't add signficant time
# to getting the container running if it's already up-to-date
shell: update-image
	docker run --volume $(PWD):/srv --interactive --tty $(DOCKER_IMAGE_TAG)
