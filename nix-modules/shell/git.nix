{ config, ... }:
{
  flake.modules.homeManager.base.programs.git = {
    enable = true;
    settings.user = {
      inherit (config.flake.meta.user) name email;
    };
  };
}
