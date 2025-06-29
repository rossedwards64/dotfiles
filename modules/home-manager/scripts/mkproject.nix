{ pkgs, lib }:

pkgs.writers.writeGuileBin "mkproject"
  {
    libraries = with pkgs; [
      git
      cargo
      leiningen
      sbt
    ];
  }
  (
    builtins.replaceStrings
      [ "#!/usr/bin/env guile\n!#" ]
      [
        ""
      ]
      (lib.readFile ../../../.local/share/bin/mkproject)
  )
