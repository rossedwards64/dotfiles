{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines."NixOS Wiki" =
    {
      updateInterval = 24 * 60 * 60 * 1000;
      urls = [ { template = "https://wiki.nixos.org/wiki/{searchTerms}"; } ];
      definedAliases = [ "@nixwiki" ];
      icon = "https://wiki.nixos.org/nixos.png";
    };
}
