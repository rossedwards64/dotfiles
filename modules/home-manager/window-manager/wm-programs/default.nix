{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
let
  cfg = config.modules.wm-programs;
in
{
  imports = [
    ./fuzzel
    ./swayidle
    ./swaylock
    ./swaync
    ./waybar
    ./wob
  ];

  options.modules.wm-programs = {
    enable = mkEnableOption "wm-programs";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      brightnessctl
      swaynotificationcenter
    ];

    programs = {
      imv.enable = true;
    };

    services = {
      udiskie = {
        enable = true;
        tray = "always";
        notify = true;
        automount = true;
      };
    };

    modules = {
      fuzzel.enable = true;
      swayidle.enable = true;
      swaylock.enable = true;
      swaync.enable = true;
      waybar.enable = true;
      wob.enable = true;
    };
  };
}
