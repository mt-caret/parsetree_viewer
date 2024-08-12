.DEFAULT_GOAL := build
MAKEFLAGS += --jobs=2

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: build
build:
	opam exec -- dune build --release parsetree_viewer.bc.js index.html *.css

.PHONY: dev
dev:
	opam exec -- dune build -w parsetree_viewer.bc.js index.html *.css @fmt @default

.PHONY: serve
serve:
	python -m http.server -d _build/default

dist: build
	mkdir -p dist
	cp _build/default/{parsetree_viewer.bc.js,index.html,*.css} dist/
