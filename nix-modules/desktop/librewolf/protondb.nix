{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines.ProtonDB =
    let
      url = "https://protondb.com";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "@protondb" ];
      icon = "${url}/favicon.ico";
      urls = [
        {
          template = "${url}/search";
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
