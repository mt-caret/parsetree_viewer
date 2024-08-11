(lang dune 3.16)

(name parsetree_viewer)

(generate_opam_files true)

(source
 (github mt-caret/parsetree_viewer))

(authors mtakeda)

(maintainers mtakeda)

(package
 (name parsetree_viewer)
 (synopsis "Simple GUI for showing sexp representations of the parsetree")
 (allow_empty)
 (depends
  async_kernel
  core
  incr_dom
  incremental
  ppx_jane
  ppxlib
  virtual_dom
  (ocamlformat
   (and
    (= 0.26.2)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)))
