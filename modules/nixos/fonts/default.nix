{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.fonts;
in {
  options.modules.fonts = { enable = mkEnableOption "fonts"; };

  config = mkIf cfg.enable {
    fonts = {
      packages = with pkgs; [
        iosevka
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        source-code-pro
        source-han-mono
        source-han-sans
        source-han-serif
        wqy_zenhei
        (nerdfonts.override { fonts = [ "Iosevka" ]; })
      ];

      fontDir.enable = true;
      fontconfig = {
        enable = true;

        defaultFonts = {
          serif = [ "Iosevka Slab" ];
          sansSerif = [ "Iosevka SS08" ];
          monospace = [ "Iosevka" ];
        };
      };
    };
  };
}
