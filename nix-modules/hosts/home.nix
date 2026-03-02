{ config, ... }:
let
  inherit (config.flake.meta.user) username;
in
{
  flake.modules.homeManager.base = {
    programs.home-manager.enable = true;
    xdg.enable = true;

    home = {
      inherit username;
      homeDirectory = "/home/${username}";
    };
  };
}
