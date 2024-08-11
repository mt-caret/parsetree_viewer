.PHONY: build
build:
	opam exec -- dune build --profile=release
ifdef CI
	yarn build:github-pages
else
	yarn build
endif

.PHONY: fmt
fmt:
	opam exec -- dune build --auto-promote @fmt
