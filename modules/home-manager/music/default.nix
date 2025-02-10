{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.music;
in
{
  options.modules.music = {
    enable = mkEnableOption "music";
  };

  config = mkIf cfg.enable {
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
