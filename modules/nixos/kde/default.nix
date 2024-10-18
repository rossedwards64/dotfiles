{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.kde;
in
{
  options.modules.kde = {
    enable = mkEnableOption "kde";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs.kdePackages; [
      kdeconnect-kde
      kdenlive
      kget
      kiten
      kmag
      kteatime
      partitionmanager
      powerdevil
      tokodon
    ];

    environment = {
      plasma6.excludePackages = with pkgs; [
        konsole
        xterm
      ];
    };

    services = {
      displayManager = {
        defaultSession = "plasma";
        sddm.enable = true;
        sddm.wayland.enable = true;
      };

      desktopManager = {
        plasma6 = {
          enable = true;
          enableQt5Integration = true;
        };
      };
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-kde
      ];
    };
  };
}
