{ config, ... }:
let
  stylix.fonts.sizes = {
    applications = 14;
    terminal = 12;
    desktop = 10;
    popups = 16;
  };
in
{
  configurations.nixos.ross-desktop.module = { inherit stylix; };
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module = {
    inherit stylix;
  };
}
