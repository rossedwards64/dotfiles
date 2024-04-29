{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.theme;
in {
  options.modules.theme = { enable = mkEnableOption "theme"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      catppuccin
      catppuccin-cursors
      catppuccin-gtk
      catppuccin-kde
      catppuccin-sddm-corners
      rose-pine-icon-theme
    ];

    gtk = with pkgs; {
      enable = true;

      gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

      font.name = "Iosevka NF";

      theme = {
        package = catppuccin-gtk;
        name = "Catppuccin-Dark";
      };

      iconTheme = {
        package = rose-pine-icon-theme;
        name = "oomox-rose-pine-moon";
      };

      cursorTheme = {
        package = catppuccin-cursors.mochaDark;
        name = "Catppuccin-Mocha-Dark-Cursors";
      };
    };

    qt = with pkgs; {
      enable = true;
      platformTheme.name = "kde";
      style.package = catppuccin-kde;
    };
  };
}
