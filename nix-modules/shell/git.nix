{ config, ... }:
{
  flake.modules.homeManager.base.programs.git = {
    enable = true;
    settings.user = {
      name = config.flake.meta.user.name;
      email = config.flake.meta.user.email;
    };
  };
}
