{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines."Arch Linux Wiki" =
    let
      url = "https://wiki.archlinux.org";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "@archlinux" ];
      icon = "${url}/favicon.ico";
      urls = [
        {
          template = "${url}/title/{searchTerms}";
        }
      ];
    };
}
