{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.music;
in {
  options.modules.music = { enable = mkEnableOption "music"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ mpd ncspot spotify ];

    services.mpd = {
      enable = true;
      dataDir = "${config.xdg.dataHome}/mpd";
    };

    programs.ncspot = {
      enable = true;

      settings = {

      };
    };
  };
}
