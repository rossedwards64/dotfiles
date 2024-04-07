{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.kde;
in {
  options.modules.kde = { enable = mkEnableOption "kde"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs.kdePackages; [
      bluez-qt
      discover
      kdeconnect-kde
      kdenlive
      kget
      kio
      kio-extras
      kio-fuse
      kiten
      kmag
      kteatime
      partitionmanager
      plasma-nm
      plasma-pa
      plasma-workspace
      powerdevil
      tokodon
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
