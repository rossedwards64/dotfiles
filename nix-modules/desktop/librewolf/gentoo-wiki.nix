{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines."Gentoo Wiki" =
    let
      url = "https://wiki.gentoo.org";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "@gentoo" ];
      icon = "${url}/favicon.ico";
      urls = [
        {
          template = "${url}/wiki/{searchTerms}";
        }
      ];
    };
}
