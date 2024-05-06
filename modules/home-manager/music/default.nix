{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.music;
in {
  options.modules.music = { enable = mkEnableOption "music"; };

  config = mkIf cfg.enable {
    programs = { spotify-player = { enable = true; }; };

    services.mpd = {
      enable = true;
      dataDir = "${config.xdg.dataHome}/mpd";
    };
  };
}
