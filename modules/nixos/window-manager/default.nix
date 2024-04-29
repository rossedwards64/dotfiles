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
    programs.sway.enable = true;

    services.greetd = {
      enable = true;

      settings = {
        default_session = {
          command =
            "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd ${wm} -t -g 'WELCOME TO WORM LINUX' --asterisks";
          user = "ross";
        };
      };
    };
  };
}
