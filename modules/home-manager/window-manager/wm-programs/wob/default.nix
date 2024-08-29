{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.wob;
  white = "ffffffff";
  black = "000000ff";
  red = "ff0000ff";
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
          border_color = "${white}";
          background_color = "${black}";
          bar_color = "${white}";
          overflow_border_color = "${white}";
          overflow_background_color = "${black}";
          overflow_bar_color = "${red}";
          output_mode = "focused";
        };
      };
    };
  };
}
