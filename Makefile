ifeq ($(wildcard rebar3),rebar3)
	REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")
HOSTNAME = $(shell hostname)

ifeq ($(REBAR3),)
	REBAR3 = $(CURDIR)/rebar3
endif

rebar_cmd = $(REBAR3) $(profile:%=as %)

.PHONY: all
all: deps

.PHONY: app
app:
	@$(rebar_cmd) compile

.PHONY: deps
deps:
	@$(rebar_cmd) do get-deps compile

.PHONY: fetch-deps
fetch-deps:
	@$(rebar_cmd) get-deps

.PHONY: list-deps
list-deps:
	@$(rebar_cmd) tree

.PHONY: rel
rel:
	@$(rebar_cmd) release

.PHONY: docs
docs:
	@$(rebar_cmd) edoc

.PHONY: tests
tests:
	@$(rebar_cmd) do xref, eunit, ct, cover

.PHONY: clean
clean:
	@$(rebar_cmd) clean

.PHONY: distclean
distclean: clean
	@$(rebar_cmd) delete-deps

.PHONY: plt
plt:
	@$(rebar_cmd) dialyzer -u

.PHONY: dialyze
dialyze:
	@$(rebar_cmd) dialyzer

.PHONY: eunit
eunit:
	@$(rebar_cmd) eunit

.PHONY: ct
ct:
	sed -i "s/%HOSTNAME%/${HOSTNAME}/g" env/test.config && \
	$(rebar_cmd) ct && \
	git checkout env/test.config

.PHONY: xref
xref:
	@$(rebar_cmd) xref

.PHONY: cover-report
cover-report:
	@$(rebar_cmd) cover

.PHONY: release
release:
	@$(rebar_cmd) release
