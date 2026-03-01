{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module.home.stateVersion =
    "23.11";
}
