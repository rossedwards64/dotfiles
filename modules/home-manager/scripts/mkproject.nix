{ pkgs, lib }:

pkgs.writers.writeGuileBin "mkproject" { } (
  builtins.replaceStrings
    [ "#!/usr/bin/env guile\n!#" "git init" "cargo new" "lein new" "sbt new" ]
    [
      ""
      "${pkgs.git}/bin/git init"
      "${pkgs.cargo}/bin/cargo new"
      "${pkgs.leiningen}/bin/lein new"
      "${pkgs.sbt}/bin/sbt new"
    ]
    (lib.readFile ../../../.local/share/bin/mkproject)
)
