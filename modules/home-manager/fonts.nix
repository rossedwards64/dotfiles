{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.fonts;
in {
  options.modules.fonts = { enable = mkEnableOption "fonts"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      iosevka
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
    ];
  };
}
