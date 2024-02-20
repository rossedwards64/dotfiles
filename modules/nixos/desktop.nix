{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.desktop;
in {
  options.modules.desktop = { enable = mkEnableOption "desktop"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      flatpak
      gnome.gnome-software
      neovim
      wayland
      winetricks
      xwayland
    ];

    services = {
      xserver = {
        enable = true;

        displayManager = {
          # kde
          defaultSession = "plasmawayland";
          sddm.enable = true;
          sddm.wayland.enable = true;

          # gnome
          # defaultSession = "gnome";
          # gdm.enable = true;
        };

        desktopManager = {
          # kde
          plasma5.enable = true;

          # gnome
          # gnome.enable = true;
        };

        xkb = {
          layout = "gb";
          variant = "";
          options = "ctrl:nocaps";
        };
      };

      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-kde ];
      config.common.default = "gtk";
    };
  };
}