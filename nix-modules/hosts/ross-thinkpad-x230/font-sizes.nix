{ config, ... }:
let
  stylix.fonts.sizes = {
    applications = 12;
    terminal = 12;
    desktop = 10;
    popups = 14;
  };
in
{
  configurations.nixos.ross-thinkpad-x230.module = { inherit stylix; };
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module = {
    inherit stylix;
  };
}
