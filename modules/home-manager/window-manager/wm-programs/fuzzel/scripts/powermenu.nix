{ pkgs, lib }:

pkgs.writers.writeGuileBin "powermenu"
  {
    libraries = with pkgs; [
      fuzzel
      procps
      coreutils
      systemd
      playerctl
      systemd
      coreutils
    ];
  }
  (
    builtins.replaceStrings [ "#!/usr/bin/env guile\n!#" ] [ "" ] (
      lib.readFile ../../../../../../.config/fuzzel/scripts/powermenu
    )
  )
