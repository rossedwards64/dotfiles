{ lib, ... }:
let
  mainFont = "Iosevka NF";
  stylixConfig =
    { pkgs, ... }:
    let
      font = {
        package = pkgs.nerd-fonts.iosevka;
        name = mainFont;
      };
    in
    {
      stylix.fonts = {
        monospace = {
          inherit (font) package name;
        };
        sansSerif = {
          inherit (font) package name;
        };
        serif = {
          inherit (font) package name;
        };
        emoji = {
          package = pkgs.noto-fonts-color-emoji;
          name = "Noto Emoji";
        };
      };
    };
in
{
  flake.meta.font = mainFont;

  flake.modules = {
    homeManager.base = stylixConfig;

    nixos.base =
      { pkgs, ... }:
      {
        imports = [ stylixConfig ];
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
            localConf = lib.readFile ../../.config/fontconfig/fonts.conf;

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
  };
}
