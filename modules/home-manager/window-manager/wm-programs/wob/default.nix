{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.wob;
in
{
  options.modules.wob = {
    enable = mkEnableOption "wob";
  };

  config = mkIf cfg.enable {
    services.wob = {
      enable = true;
      systemd = true;

      settings = {
        "" = {
          timeout = 500;
          max = 100;
          anchor = "bottom center";
          margin = 100;
          border_offset = 2;
          border_size = 2;
          bar_padding = 5;
          overflow_mode = "nowrap";
          height = 30;
          width = 300;
        };
      };
    };
  };
}
