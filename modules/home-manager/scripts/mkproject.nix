{ pkgs, lib }:

let
  programs = lib.makeBinPath (
    with pkgs;
    [
      git
      cargo
      leiningen
      sbt
      cabal-install
      haskellPackages.ghc
    ]
  );
in
pkgs.writers.writeGuileBin "mkproject" { } (
  builtins.replaceStrings
    [ "#!/usr/bin/env guile\n!#" ]
    [
      "(setenv \"PATH\" \"${programs}\")"
    ]
    (lib.readFile ../../../.local/share/bin/mkproject)
)
