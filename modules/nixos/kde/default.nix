{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.kde;
in {
  options.modules.kde = { enable = mkEnableOption "kde"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs.kdePackages; [
      kiten
      discover
      kdeconnect-kde
      kdenlive
      kget
      kmag
      kteatime
      partitionmanager
      plasma-nm
      plasma-pa
      powerdevil
      tokodon
      plasma-workspace
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
