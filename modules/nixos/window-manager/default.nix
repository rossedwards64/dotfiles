{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.window-manager;
  wm = "sway";
  wm-bin = "${pkgs.sway}/bin/${wm}";
in {
  options.modules.window-manager = {
    enable = mkEnableOption "window-manager";
  };

  config = mkIf cfg.enable {
    programs.${wm}.enable = true;

    services.greetd = {
      enable = true;

      settings = rec {
        initial_session = {
          command = "${wm-bin}";
          user = "ross";
        };
        default_session = initial_session;
      };
    };
  };
}
