{ config, ... }:
{
  flake.modules.homeManager.base.programs.librewolf.profiles.${config.flake.meta.user.username}.search.engines."Dwarf Fortress Wiki" =
    let
      url = "https://dwarffortresswiki.org";
    in
    {
      updateInterval = 24 * 60 * 60 * 1000;
      definedAliases = [ "@dwarffortress" ];
      icon = "${url}/favicon.ico";
      urls = [ { template = "${url}/index.php/{searchTerms}"; } ];
    };
}
