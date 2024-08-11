.DEFAULT_GOAL := build
MAKEFLAGS += --jobs=2

.PHONY: build
build:
	opam exec -- dune build --profile=release
	yarn build

.PHONY: dev-ocaml
dev-ocaml:
	opam exec -- dune build --watch

.PHONY: dev-parcel
dev-parcel:
	yarn dev

.PHONY: dev
dev: dev-ocaml dev-parcel

.PHONY: fmt
fmt:
	opam exec -- dune build --auto-promote @fmt