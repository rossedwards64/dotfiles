{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.gnome;
in {
  options.modules.gnome = { enable = mkEnableOption "gnome"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ gnome.gnome-software ];

    services = {
      xserver = {
        displayManager = {
          defaultSession = "gnome";
          gdm.enable = true;
        };

        desktopManager = { gnome.enable = true; };
      };
    };
  };
}
