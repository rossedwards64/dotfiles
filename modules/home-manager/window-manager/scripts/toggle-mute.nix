{ pkgs, lib }:

pkgs.writers.writeGuileBin "toggle-mute" { } (
  builtins.replaceStrings
    [ "#!/usr/bin/env guile\n!#" "wpctl" "notify-send" ]
    [ "" "${pkgs.wireplumber}/bin/wpctl" "${pkgs.libnotify}/bin/notify-send" ]
    (lib.readFile ../../../../.local/share/bin/toggle-mute)
)
