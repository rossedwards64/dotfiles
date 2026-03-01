{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module.home.stateVersion =
    "23.11";
}
