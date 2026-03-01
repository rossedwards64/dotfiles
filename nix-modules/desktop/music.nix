{
  flake.modules.homeManager.base =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [ spot ];

      services = {
        playerctld.enable = true;

        mpd = {
          enable = true;
          dataDir = "${config.xdg.dataHome}/mpd";
        };
      };
    };
}
