{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    let
      inherit (config.flake.meta.windowManager) modifier;
    in
    { pkgs, ... }:
    {
      home.packages = [
        config.flake.scripts.screenshot
      ];

      wayland.windowManager.sway = {
        enable = true;
        checkConfig = true;
        xwayland = true;
        wrapperFeatures = {
          base = true;
          gtk = true;
        };

        systemd = {
          enable = true;
          xdgAutostart = true;
          variables = [ "--all" ];
        };

        swaynag = {
          enable = true;
        };

        config = {
          inherit modifier;
          inherit (config.flake.meta.windowManager)
            down
            right
            left
            up
            ;
          bindkeysToCode = false;
          defaultWorkspace = "workspace number 1";
          menu = "${lib.getExe pkgs.fuzzel}";
          terminal = "${lib.getExe pkgs.alacritty}";
          workspaceAutoBackAndForth = true;
          workspaceLayout = "tabbed";
          bars = [ ];
          window.border = 1;
          seat."seat0".xcursor_theme = "catppuccin-mocha-dark-cursors 24";

          fonts = {
            names = [ config.flake.meta.font ];
            style = "Regular";
          };

          gaps = {
            inner = 5;
            outer = 5;
            smartBorders = "off";
            smartGaps = false;
          };

          input = {
            "1:1:AT_Translated_Set_2_keyboard" = {
              xkb_layout = "gb";
              xkb_options = "ctrl:nocaps";
            };

            "12625:16386:ROYUAN_EPOMAKER_TH66_Keyboard" = {
              xkb_layout = "us";
              xkb_options = "ctrl:nocaps,altwin";
            };

            "2:7:SynPS/2_Synaptics_TouchPad" = {
              dwt = "enabled";
              tap = "enabled";
              natural_scroll = "enabled";
              middle_emulation = "enabled";
            };

            "1356:2508:Sony_Interactive_Entertainment_Wireless_Controller_Touchpad" = {
              events = "disabled";
            };
          };

          floating = {
            inherit modifier;
            border = 1;
            criteria = [ ];
            titlebar = true;
          };

          focus = {
            followMouse = "always";
            mouseWarping = false;
            newWindow = "urgent";
          };
        };
      };
    };
}
