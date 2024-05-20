{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.fuzzel;
in {
  options.modules.fuzzel = { enable = mkEnableOption "fuzzel"; };

  config = mkIf cfg.enable {
    programs.fuzzel = {
      enable = true;

      settings = {
        main = {
          font = mkForce "Iosevka NF:size=16";
          icon-theme = "rose-pine-moon";
          terminal = "${pkgs.alacritty}/bin/alacritty";
          anchor = "center";
          exit-on-keyboard-focus-loss = false;
        };

        border = {
          width = 3;
          radius = 20;
        };
      };
    };
  };
}
