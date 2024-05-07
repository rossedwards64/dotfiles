{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.window-manager;
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
              ${pkgs.greetd.tuigreet}/bin/tuigreet --cmd ${pkgs.hyprland}/bin/Hyprland -t -g \
               'WELCOME TO WORM LINUX' --asterisks
            '';
            user = "ross";
          };
        };
      };
    };

    security.pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };
  };
}
