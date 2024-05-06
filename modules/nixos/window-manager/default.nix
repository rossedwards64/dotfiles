{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.window-manager;
  wm = "${pkgs.sway}/bin/sway";
in {
  options.modules.window-manager = {
    enable = mkEnableOption "window-manager";
  };

  config = mkIf cfg.enable {
    services = {
      udisks2.enable = true;

      greetd = {
        enable = true;

        settings = {
          default_session = {
            command = ''
              ${pkgs.greetd.tuigreet}/bin/tuigreet --cmd ${wm} -t -g \
               'WELCOME TO WORM LINUX' --asterisks
            '';
            user = "ross";
          };
        };
      };

      actkbd = {
        enable = true;
        bindings = [
          {
            keys = [ 224 ];
            events = [ "key" ];
            command = "${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
          }
          {
            keys = [ 225 ];
            events = [ "key" ];
            command = "${pkgs.brightnessctl}/bin/brightnessctl set 5%+";
          }
        ];
      };
    };

    security.pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };
  };
}
