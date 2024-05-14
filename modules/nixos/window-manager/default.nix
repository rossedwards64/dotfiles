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
            user = "ross";
            command = ''
              ${pkgs.greetd.tuigreet}/bin/tuigreet --cmd ${pkgs.sway}/bin/sway -t -g \
                  'WELCOME TO WORM LINUX' --asterisks
            '';
          };
        };
      };
    };

    security.pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };

    environment.etc."greetd/environments".text = ''
      ${pkgs.sway}/bin/sway
      ${pkgs.hyprland}/bin/Hyprland
    '';
  };
}
