{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines."DuckDuckGo Lite" =
    let
      url = "https://lite.duckduckgo.com";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      icon = "${url}/lite/favicon.ico";
      definedAliases = [ "@ddgl" ];
      urls = [
        {
          template = url;
          params = [
            {
              name = "q";
              value = "{searchTerms}";
            }
          ];
        }
      ];
    };
}
