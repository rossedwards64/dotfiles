{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.niri.settings.spawn-at-startup = [
        {
          command = [
            "${lib.getExe' pkgs.gnome-keyring "gnome-keyring-daemon"}"
            "--start"
            "--foreground"
            "--components=pkcs11,secrets,ssh"
          ];
        }
        { command = [ "${lib.getExe pkgs.myxer}" ]; }
        {
          command = [
            "${lib.getExe pkgs.swayidle}"
            "-w"
          ];
        }
        {
          command = [
            "${lib.getExe' pkgs.wl-clipboard "wl-paste"}"
            "--type image"
            "--watch cliphist"
            "store"
          ];
        }
        {
          command = [
            "${lib.getExe' pkgs.wl-clipboard "wl-paste"}"
            "--type text"
            "--watch cliphist"
            "store"
          ];
        }
        { command = [ "${lib.getExe pkgs.alacritty}" ]; }
        { command = [ "${lib.getExe pkgs.librewolf}" ]; }
        { command = [ "${lib.getExe pkgs.lutris}" ]; }
        {
          command = [
            "${lib.getExe' config.flake.pkgs.emacs "emacsclient"}"
            "-c"
            "-a=''"
          ];
        }
      ];
    };
}
