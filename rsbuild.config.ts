import { defineConfig } from "@rsbuild/core";

export default defineConfig({
  source: {
    preEntry: "./styles/import.css",
    entry: {
      index: "./_build/default/src/parsetree_viewer.bc.js",
    },
  },
  output: {
    assetPrefix: "/parsetree_viewer/",
  },
  html: {
    title: "Parsetree Viewer",
    mountId: "app",
  },
});
