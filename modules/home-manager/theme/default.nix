{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.theme;
in {
  options.modules.theme = { enable = mkEnableOption "theme"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ catppuccin ];

    gtk = with pkgs; {
      enable = true;
      font.name = "Iosevka NF";

      theme = {
        package = catppuccin-gtk;
        name = "Catppuccin-Dark";
      };

      iconTheme = {
        package = rose-pine-icon-theme;
        name = "rose-pine-moon";
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
