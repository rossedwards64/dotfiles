{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop;
in
{
  options.modules.desktop = {
    enable = mkEnableOption "desktop";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      flatpak
      gnome-software
      neovim
      rose-pine-icon-theme
      wayland
      winetricks
      wl-clipboard
      xwayland
    ];

    services = {
      xserver = {
        enable = true;
        xkb = {
          variant = "";
          options = "ctrl:nocaps";
        };
      };

      jackett.enable = true;
    };

    xdg.portal = {
      enable = true;
      wlr.enable = true;
      config.common.default = "gtk";
    };
  };
}
