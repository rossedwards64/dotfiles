{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.kde;
in {
  options.modules.kde = { enable = mkEnableOption "kde"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      kdePackages.flatpak-kcm
      kdePackages.kcmutils
      kdePackages.plymouth-kcm
      kdePackages.sddm-kcm
    ];

    services = {
      xserver = {
        displayManager = {
          defaultSession = "plasma";
          sddm.enable = true;
          sddm.wayland.enable = true;
        };

        desktopManager = { plasma6.enable = true; };
      };
    };
  };
}
