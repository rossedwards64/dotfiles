{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.fonts;
in
{
  options.modules.fonts = {
    enable = mkEnableOption "fonts";
  };

  config = mkIf cfg.enable {
    fonts = {
      enableDefaultPackages = true;
      packages = with pkgs; [
        ipafont
        nerd-fonts.iosevka
        iosevka
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji
        source-code-pro
        source-han-mono
        source-han-sans
        source-han-serif
        wqy_zenhei
      ];

      fontDir.enable = true;
      fontconfig = {
        enable = true;
        cache32Bit = true;
        localConf = lib.readFile ../../../.config/fontconfig/fonts.conf;

        defaultFonts = {
          serif = [
            "Iosevka Slab"
            "Source Han Serif SC"
          ];
          sansSerif = [
            "Iosevka SS08"
            "Source Han Serif SC"
          ];
          monospace = [
            "Iosevka"
            "Source Han Serif SC"
            "Sarasa Mono SC"
          ];
        };
      };
    };
  };
}
