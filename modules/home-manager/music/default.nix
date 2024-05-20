{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.music;
in {
  options.modules.music = { enable = mkEnableOption "music"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ spot ];
    programs = { spotify-player.enable = true; };

    services = {
      mpd = {
        enable = true;
        dataDir = "${config.xdg.dataHome}/mpd";
      };
      playerctld.enable = true;
    };

    xdg.configFile."spotify-player/app.toml".source =
      (pkgs.formats.toml { }).generate "spotify-config" {
        themes = [
          {
            name = "Catppuccin-mocha";
            component_style = {
              selection = ''{ bg = "#313244", modifiers = ["Bold"] }'';
            };

            palette = {
              background = "#1E1E2E";
              foreground = "#CDD6F4";
              black = "#1E1E2E";
              blue = "#89B4FA";
              cyan = "#89DCEB";
              green = "#A6E3A1";
              magenta = "#CBA6F7";
              red = "#F38BA8";
              white = "#CDD6F4";
              yellow = "#F9E2AF";
              bright_black = "#1E1E2E";
              bright_blue = "#89B4FA";
              bright_cyan = "#89DCEB";
              bright_green = "#A6E3A1";
              bright_magenta = "#CBA6F7";
              bright_red = "#F38BA8";
              bright_white = "#CDD6F4";
              bright_yellow = "#F9E2AF";
            };
          }
          {
            name = "Catppuccin-latte";
            component_style = {
              selection = ''{ bg = "#CCD0DA", modifiers = ["Bold"] }'';
            };

            palette = {
              background = "#EFF1F5";
              foreground = "#4C4F69";
              black = "#EFF1F5";
              blue = "#1E66F5";
              cyan = "#04A5E5";
              green = "#40A02B";
              magenta = "#8839EF";
              red = "#D20F39";
              white = "#4C4F69";
              yellow = "#DF8E1D";
              bright_black = "#EFF1F5";
              bright_blue = "#1E66F5";
              bright_cyan = "#04A5E5";
              bright_green = "#40A02B";
              bright_magenta = "#8839EF";
              bright_red = "#D20F39";
              bright_white = "#4C4F69";
              bright_yellow = "#DF8E1D";
            };
          }
          {
            name = "Catppuccin-frappe";
            component_style = {
              selection = ''{ bg = "#414559", modifiers = ["Bold"] }'';
            };

            palette = {
              background = "#303446";
              foreground = "#C6D0F5";
              black = "#303446";
              blue = "#8CAAEE";
              cyan = "#89DCEB";
              green = "#A6D189";
              magenta = "#CA9EE6";
              red = "#E78284";
              white = "#C6D0F5";
              yellow = "#E5C890";
              bright_black = "#303446";
              bright_blue = "#8CAAEE";
              bright_cyan = "#89DCEB";
              bright_green = "#A6D189";
              bright_magenta = "#CA9EE6";
              bright_red = "#E78284";
              bright_white = "#C6D0F5";
              bright_yellow = "#E5C890";
            };
          }
          {
            name = "Catppuccin-macchiato";
            component_style = {
              selection = ''{ bg = "#363A4F", modifiers = ["Bold"] }'';
            };

            palette = {
              background = "#24273A";
              foreground = "#CAD3F5";
              black = "#24273A";
              blue = "#8AADF4";
              cyan = "#91D7E3";
              green = "#A6DA95";
              magenta = "#C6A0F6";
              red = "#ED8796";
              white = "#CAD3F5";
              yellow = "#EED49F";
              bright_black = "#24273A";
              bright_blue = "#8AADF4";
              bright_cyan = "#91D7E3";
              bright_green = "#A6DA95";
              bright_magenta = "#C6A0F6";
              bright_red = "#ED8796";
              bright_white = "#CAD3F5";
              bright_yellow = "#EED49F";
            };
          }
        ];
      };
  };
}
