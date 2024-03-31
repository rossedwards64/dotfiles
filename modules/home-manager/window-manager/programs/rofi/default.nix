{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.rofi;
in {
  options.modules.rofi = { enable = mkEnableOption "rofi"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ rofi-wayland ];

    programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      location = "center";
      font = "Iosevka NF";
    };
  };
}
