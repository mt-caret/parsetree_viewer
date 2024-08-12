SHELL := /bin/bash
.DEFAULT_GOAL := build

.PHONY: clean
clean:
	rm -rf dist
	opam exec -- dune clean

.PHONY: build
build:
	opam exec -- dune build --release src/{parsetree_viewer.bc.js,index.html,*.css}

.PHONY: dev
dev:
	opam exec -- dune build -w src/{parsetree_viewer.bc.js,index.html,*.css} @fmt @default

.PHONY: serve
serve:
	python -m http.server -d _build/default/src

dist: build
	mkdir -p dist
	cp _build/default/src/{parsetree_viewer.bc.js,index.html,*.css} dist/
