{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.fuzzel;
in {
  options.modules.fuzzel = { enable = mkEnableOption "fuzzel"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ fuzzel ];

    programs.fuzzel = {
      enable = true;

      settings = {
        main = {
          font = "Iosevka NF:size=12";
          icon-theme = "rose-pine-moon";
          terminal = "${pkgs.alacritty}/bin/alacritty";
          anchor = "center";
          exit-on-keyboard-focus-loss = false;
        };

        border = {
          width = 3;
          radius = 20;
        };

        # catppuccin mocha colours
        colors = {
          background = "1e1e2edd";
          text = "cdd6f4ff";
          match = "f38ba8ff";
          selection = "575b70ff";
          selection-match = "f38ba8ff";
          selection-text = "cdd6f4ff";
          border = "b4befeff";
        };
      };
    };
  };
}
