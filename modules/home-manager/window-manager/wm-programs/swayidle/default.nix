{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.swayidle;
in {
  options.modules.swayidle = { enable = mkEnableOption "swayidle"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ swayidle ];

    services.swayidle = {
      enable = true;

      events = [
        {
          event = "before-sleep";
          command = "${pkgs.swaylock}/bin/swaylock -fF";
        }
        {
          event = "after-resume";
          command = "hyprctl dispatch dpms on";
        }
      ];

      timeouts = [
        {
          timeout = 300;
          command = "${pkgs.swaylock}/bin/swaylock -fF";
        }
        {
          timeout = 600;
          command = "hyprctl dispatch dpms off";
        }
      ];
    };
  };
}
