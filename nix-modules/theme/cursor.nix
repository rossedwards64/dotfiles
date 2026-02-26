{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        stylix.cursor = {
          package = pkgs.catppuccin-cursors.mochaDark;
          name = "Catppuccin-Mocha-Dark-Cursors";
          size = 24;
        };
      };

    homeManager.base =
      { pkgs, ... }:
      {
        gtk.cursorTheme = {
          package = pkgs.catppuccin-cursors.mochaDark;
          name = "catppuccin-mocha-dark-cursors";
          size = 24;
        };
        stylix.cursor = {
          package = pkgs.catppuccin-cursors.mochaDark;
          name = "Catppuccin-Mocha-Dark-Cursors";
          size = 24;
        };
      };
  };
}
