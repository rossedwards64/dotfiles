{ lib, config, ... }:
with lib;
let
  cfg = config.modules.sway;
  opacity = 0.9;
  font = "Iosevka NF";
  modifier = "Mod4";
  down = "j";
  right = "l";
  left = "h";
  up = "k";
in {
  options.modules.sway = { enable = mkEnableOption "sway"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ];

    wayland = {
      windowManager = {
        sway = {
          enable = true;
          xwayland = true;

          systemd = { enable = true; };

          swaynag = { enable = true; };

          config = {
            inherit modifier down right left up;
            bindkeysToCode = false;
            defaultWorkspace = "workspace number 1";
            menu = "";
            terminal = "alacritty";
            workspaceAutoBackAndForth = true;
            workspaceLayout = "tabbed";
            workspaceOutputAssign = { };
            assigns = {
              "1: emacs" = [{ app_id = "^emacs(client)?$"; }];
              "2: alacritty" = [{ app_id = "^Alacritty$"; }];
              "3: browser" = [{ app_id = "^firefox$"; }];
              "4: discord" = [{ class = "^discord$"; }];
              "5: spotify" = [{ app_id = "^(dev.alextren.)?Spot(ify)?$"; }];
              "6: launchers" = [{ class = "^steam$"; }];
              "7: steam game" = [{ class = "^steam_app_[0-9]+$"; }];
              "7: game" = [{
                class =
                  "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";
                app_id =
                  "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";
              }];
            };

            output = { };
            window = { };
            startup = { };

            floating = {
              inherit modifier;
              border = 1;
              criteria = [ ];
              titlebar = true;
            };

            focus = {
              followMouse = yes;
              mouseWarping = true;
              newWindow = "urgent";
            };

            seat = { };
            bars = { };
            colors = { };

            fonts = {
              names = [ font ];
              style = "Regular";
              size = 10.0;
            };

            gaps = {
              smartBorders = "off";
              smartGaps = true;
            };

            input = {
              "${modifier}+${down}" = "focus down";
              "${modifier}+${left}" = "focus left";
              "${modifier}+${right}" = "focus right";
              "${modifier}+${up}" = "focus up";
            };

            keyboard = { };
            extraConfigEarly = "";
            extraConfig = "";
            extraSessionCommands = "";
            extraOptions = [ ];
          };
        };
      };
    };
  };
}
