{ pkgs, lib }:

pkgs.writers.writeGuileBin "toggle-mute"
  {
    libraries = with pkgs; [
      libnotify
      wireplumber
    ];
  }
  (
    builtins.replaceStrings [ "#!/usr/bin/env guile\n!#" ] [ "" ] (
      lib.readFile ../../../../.local/share/bin/toggle-mute
    )
  )
