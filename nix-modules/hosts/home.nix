{ config, ... }:
let
  username = config.flake.meta.user.username;
in
{
  flake.modules.homeManager.base = {
    programs.home-manager.enable = true;
    home = {
      inherit username;
      homeDirectory = "/home/${username}";
      stateVersion = "23.11";
    };
  };
}
