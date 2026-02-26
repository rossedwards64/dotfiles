{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      stylix.icons = {
        enable = false;
        package = pkgs.rose-pine-icon-theme;
      };
    };
}
