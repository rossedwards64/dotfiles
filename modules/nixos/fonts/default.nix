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

        localConf = ''
          <?xml version="1.0" encoding="UTF-8"?>
          <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
          <fontconfig>
            <match target="font">
             <edit mode="assign" name="rgba">
              <const>none</const>
             </edit>
            </match>
            <match target="font">
             <edit mode="assign" name="hinting">
              <bool>false</bool>
             </edit>
            </match>
            <match target="font">
             <edit mode="assign" name="autohint">
              <bool>false</bool>
             </edit>
            </match>
            <match target="font">
             <edit mode="assign" name="hintstyle">
              <const>hintnone</const>
             </edit>
            </match>
            <match target="font">
             <edit mode="assign" name="antialias">
              <bool>true</bool>
             </edit>
            </match>
            <match target="font">
             <edit mode="assign" name="lcdfilter">
              <const>lcddefault</const>
             </edit>
            </match>
          </fontconfig>
        '';
      };
    };
  };
}
