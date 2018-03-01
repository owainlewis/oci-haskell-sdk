all: build

.PHONY: build
build:
	@stack build

.PHONY: test
test:
	@stack test
