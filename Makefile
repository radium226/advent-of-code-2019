#!/usr/bin/env make

SHELL=/bin/bash
.SHELLFLAGS = -e -c
.ONESHELL:

LANG := scala

include make/$(LANG).mk

.PHONY: all
all:
	find . -type f -name '$(MAIN_FILE)' -printf '%h\n' | \
		xargs -I {} basename "{}" | \
		grep -E '^[0-9]{2}$$' | \
		sort -h | \
		xargs -I {} sh -c "make day-{} LANG='$(LANG)' || exit 255"

.PHONY: day-%
day-%:
	cd "$(shell echo '$@' | sed 's,day-,,g' )"
	toilet -f "bigmono12" "$@"
	$(call run-$(LANG),$(MAIN_FILE))
	echo
