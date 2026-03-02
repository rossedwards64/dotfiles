{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines.Marginalia =
    let
      url = "https://search.marginalia.nu";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "@marginalia" ];
      icon = "${url}/favicon.ico";
      urls = [
        {
          template = "${url}/search";
          params = [
            {
              name = "query";
              value = "{searchTerms}";
            }
          ];
        }
      ];
    };
}
