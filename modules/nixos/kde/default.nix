{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.kde;
in {
  options.modules.kde = { enable = mkEnableOption "kde"; };

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

    environment = { plasma5.excludePackages = with pkgs; [ konsole xterm ]; };

    services = {
      xserver = {
        displayManager = {
          defaultSession = "plasma";
          sddm.enable = true;
          sddm.wayland.enable = true;
        };

        desktopManager = {
          plasma5 = {
            enable = true;
            runUsingSystemd = true;
          };
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
