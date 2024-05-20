{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.swaylock;
in {
  options.modules.swaylock = { enable = mkEnableOption "swaylock"; };

  config = mkIf cfg.enable {
    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;

      settings = {
        font = "Iosevka NF";
        font-size = 36;
        daemonize = true;
        clock = true;
        indicator = true;
        screenshots = true;
        grace = 5;
        hide-keyboard-layout = true;
        indicator-radius = 100;
        indicator-thickness = 7;
        fade-in = 0.2;
        effect-blur = "7x5";
        effect-vignette = "0.5:0.5";
      };
    };
  };
}
