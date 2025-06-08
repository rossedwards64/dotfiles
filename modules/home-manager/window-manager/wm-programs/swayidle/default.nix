{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.swayidle;
in
{
  options.modules.swayidle = {
    enable = mkEnableOption "swayidle";
  };

  config = mkIf cfg.enable {
    services.swayidle = {
      enable = true;

      events = [
        {
          event = "before-sleep";
          command = "${pkgs.playerctl}/bin/playerctl pause; ${pkgs.swaylock-effects}/bin/swaylock -fF";
        }
        {
          event = "after-resume";
          command = "${pkgs.sway}/bin/swaymsg 'output * power on'";
        }
      ];

      timeouts = [
        {
          timeout = 300;
          command = "${pkgs.swaylock-effects}/bin/swaylock -fF";
        }
      ];
    };
  };
}
