{ pkgs, lib }:
pkgs.writers.writeGuileBin "powermenu" { } (
  builtins.replaceStrings
    [
      "#!/usr/bin/env guile\n!#"
      "fuzzel"
      "uptime"
      "wc"
      "systemctl"
      "playerctl"
      "loginctl"
      "id -u"
    ]
    [
      ""
      "${pkgs.fuzzel}/bin/fuzzel"
      "${pkgs.procps}/bin/uptime"
      "${pkgs.coreutils}/bin/wc"
      "${pkgs.systemd}/bin/systemctl"
      "${pkgs.playerctl}/bin/playerctl"
      "${pkgs.systemd}/bin/loginctl"
      "${pkgs.coreutils}/bin/id -u"
    ]
    (lib.readFile ../../../../../../.config/fuzzel/scripts/powermenu)
)
