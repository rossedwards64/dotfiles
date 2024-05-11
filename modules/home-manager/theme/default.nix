{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.theme;
in {
  options.modules.theme = { enable = mkEnableOption "theme"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ catppuccin ];

    gtk = {
      enable = true;
      font.name = "Iosevka NF";

      theme = {
        package = pkgs.catppuccin-gtk;
        name = "Catppuccin-Dark";
      };

      iconTheme = {
        package = pkgs.rose-pine-icon-theme;
        name = "rose-pine-moon";
      };

      cursorTheme = {
        package = pkgs.catppuccin-cursors.mochaDark;
        name = "Catppuccin-Mocha-Dark-Cursors";
      };
    };

    qt = {
      enable = true;
      platformTheme.name = "kde";
      style.package = pkgs.catppuccin-kde;
    };
  };
}
