{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      stylix.icons = {
        enable = true;
        package = pkgs.rose-pine-icon-theme;
      };
    };

  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ rose-pine-icon-theme ];
      stylix.icons = {
        enable = false;
        package = pkgs.rose-pine-icon-theme;
      };
    };
}
