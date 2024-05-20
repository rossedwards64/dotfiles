{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    clj-kondo
    cljfmt
    clojure
    clojure-lsp
    leiningen
  ];
}
