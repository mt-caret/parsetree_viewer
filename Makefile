.PHONY: build
build:
	opam exec -- dune build --profile=release
	yarn build

.PHONY: fmt
fmt:
	opam exec -- dune build --auto-promote @fmt
