{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.swaylock;
  white = "00000000";
  text = "11111b";
  colour = "cba6f7";
  verColour = "89b4fa";
  wrongColour = "f38ba8";
  clearColour = "a6e3a1";
in {
  options.modules.swaylock = { enable = mkEnableOption "swaylock"; };

  config = mkIf cfg.enable {
    programs.swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;

      settings = {
        font = "Iosevka";
        font-size = 36;
        daemonize = true;
        clock = true;
        indicator = true;
        screenshots = true;
        grace = 5;
        hide-keyboard-layout = true;
        indicator-radius = 100;
        indicator-thickness = 7;
        ring-color = "${colour}";
        ring-ver-color = "${verColour}";
        ring-wrong-color = "${wrongColour}";
        ring-clear-color = "${clearColour}";
        inside-color = "${colour}";
        inside-ver-color = "${verColour}";
        inside-wrong-color = "${wrongColour}";
        inside-clear-color = "${clearColour}";
        key-hl-color = "1e1e2e";
        bs-hl-color = "eba0ac";
        text-color = "${text}";
        text-caps-lock-color = "${text}";
        line-color = "${white}";
        line-ver-color = "${white}";
        line-wrong-color = "${white}";
        line-clear-color = "${white}";
        separator-color = "${white}";
        color = "${white}";
        fade-in = 0.2;
        effect-blur = "7x5";
        effect-vignette = "0.5:0.5";
      };
    };
  };
}
