{ config, ... }:
{
  flake.modules.homeManager.base = {
    services.swaync.enable = true;
  };
}
