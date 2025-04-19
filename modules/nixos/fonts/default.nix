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
        noto-fonts-emoji
        source-code-pro
        source-han-mono
        source-han-sans
        source-han-serif
        wqy_zenhei
      ];

      fontDir.enable = true;
      fontconfig = {
        enable = true;

        defaultFonts = {
          serif = [ "Iosevka Slab" ];
          sansSerif = [ "Iosevka SS08" ];
          monospace = [ "Iosevka" ];
        };

        localConf = lib.readFile ../../../.config/fontconfig/fonts.conf;
      };
    };
  };
}
