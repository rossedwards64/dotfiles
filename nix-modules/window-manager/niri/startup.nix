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
            "${lib.getExe pkgs.swaybg}"
            "-o"
            "LVDS-1"
            "-i"
            "${pkgs.fetchurl {
              url = "https://static.zerochan.net/Lordgenome.full.198358.jpg";
              sha256 = "sha256-rh4bVRTdM9aoasomvhMQSulGwzc8DgPzP+schDK363Q=";
            }}"
          ];
        }
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
        {
          command = [
            "${lib.getExe pkgs.autotiling}"
            "-w 1 2 3 4 5 6 7 8 9 10"
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
