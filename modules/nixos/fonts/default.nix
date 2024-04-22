{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.fonts;
in {
  options.modules.fonts = { enable = mkEnableOption "fonts"; };

  config = mkIf cfg.enable {
    fonts.packages = with pkgs; [
      iosevka
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      (nerdfonts.override { fonts = [ "Iosevka" ]; })
    ];
  };
}
