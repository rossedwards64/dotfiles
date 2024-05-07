{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.sway;
  font = "Iosevka NF";
  modifier = "Mod4";
  down = "j";
  right = "l";
  left = "h";
  up = "k";
  wallpapersDir = "/home/ross/Pictures/wallpapers";

  checkMuteScript = import ../scripts/check-mute.nix { inherit pkgs; };
  toggleSinkScript = import ../scripts/toggle-sink.nix { inherit pkgs; };
  wobScript = import ../scripts/wob.nix { inherit pkgs; };

  launcher =
    import ../wm-programs/fuzzel/scripts/launcher.nix { inherit pkgs; };
  powermenu =
    import ../wm-programs/fuzzel/scripts/powermenu.nix { inherit pkgs; };
  runner = import ../wm-programs/fuzzel/scripts/runner.nix { inherit pkgs; };
  screenshot =
    import ../wm-programs/fuzzel/scripts/screenshot.nix { inherit pkgs; };
  windows = import ../wm-programs/fuzzel/scripts/windows.nix { inherit pkgs; };

  wobsock = "$XDG_RUNTIME_DIR/wob.sock";

  classAndAppId = (appName: {
    class = "${appName}";
    app_id = "${appName}";
  });

  any = ".*";
  discordRegexp = "^discord$";
  emacsRegexp = "^emacs(client)?$";
  epicGamesRegexp = "^heroic$";
  firefoxRegexp = "^firefox$";
  freetubeRegexp = "^FreeTube$";
  intellijRegexp = "^jetbrains-idea$";
  itchioRegexp = "^itch$";
  lutrisRegexp = "^lutris$";
  minecraftRegexp = "^com-atlauncher-App$";
  mpvRegexp = "^mpv$";
  spotifyRegexp = "^(dev.alextren.)?Spot(ify)?$";
  steamRegexp = "^steam$";
  steamGameRegexp = "^steam_app_[0-9]*$";
  terminalRegexp = "^Alacritty$";
  vlcRegexp = "^vlc$";
  volumeRegexp = "^pavucontrol$";
  xwvbRegexp = "^xwaylandvideobridge$";
  zathuraRegexp = "^org.pwmt.zathura$";

  gameRegexp = "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";

  colours = {
    rosewater = "#f5e0dc";
    flamingo = "#f2cdcd";
    pink = "#f5c2e7";
    mauve = "#cba6f7";
    red = "#f38ba8";
    maroon = "#eba0ac";
    peach = "#fab387";
    green = "#a6e3a1";
    teal = "#94e2d5";
    sky = "#89dceb";
    sapphire = "#74c7ec";
    blue = "#89b4fa";
    lavender = "#b4befe";
    text = "#cdd6f4";
    subtext1 = "#bac2de";
    subtext0 = "#a6adc8";
    overlay2 = "#9399b2";
    overlay1 = "#7f849c";
    overlay0 = "#6c7086";
    surface2 = "#585b70";
    surface1 = "#45475a";
    surface0 = "#313244";
    background = "#1e1e2e";
    mantle = "#181825";
    crust = "#11111b";
  };
in {
  options.modules.sway = { enable = mkEnableOption "sway"; };

  config = mkIf cfg.enable {
    wayland = {
      windowManager = {
        sway = {
          enable = true;
          checkConfig = false;
          xwayland = true;
          wrapperFeatures.gtk = true;

          systemd = {
            enable = true;
            xdgAutostart = true;
          };

          swaynag = { enable = true; };

          extraConfig = ''
            bindsym --locked {
                XF86AudioRaiseVolume exec ${wobScript}/bin/wob "-v" "-i5"
                XF86AudioLowerVolume exec ${wobScript}/bin/wob "-v" "-d5"
                XF86AudioMute exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && \
                                ${checkMuteScript}/bin/check-mute "getspeaker"
                XF86AudioMicMute exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && \
                                ${checkMuteScript}/bin/check-mute "getmic"
                XF86AudioPlay exec ${pkgs.playerctl}/bin/playerctl play-pause
                XF86AudioNext exec ${pkgs.playerctl}/bin/playerctl next
                XF86AudioPrev exec ${pkgs.playerctl}/bin/playerctl previous
                XF86MonBrightnessUp exec ${wobScript}/bin/wob "-b" "-i5"
                XF86MonBrightnessDown exec ${wobScript}/bin/wob "-b" "-d5"
            }

            bindgesture {
                pinch:inward+up move up
                pinch:inward+down move down
                pinch:inward+left move left
                pinch:inward+right move right
                swipe:right workspace prev
                swipe:left workspace next
            }
          '';

          config = {
            inherit modifier down right left up;
            bindkeysToCode = false;
            defaultWorkspace = "workspace number 1";
            menu = "${pkgs.fuzzel}/bin/fuzzel";
            terminal = "${pkgs.alacritty}/bin/alacritty";
            workspaceAutoBackAndForth = true;
            workspaceLayout = "tabbed";
            workspaceOutputAssign = [ ];
            bars = [ ];

            seat = {
              "seat0" = { xcursor_theme = "Catppuccin-Mocha-Dark-Cursors 24"; };
            };

            fonts = {
              names = [ font ];
              style = "Regular";
              size = 10.0;
            };

            gaps = {
              inner = 5;
              outer = 5;
              smartBorders = "off";
              smartGaps = true;
            };

            assigns = {
              "workspace 1" = [{ app_id = "${emacsRegexp}"; }];
              "workspace 2" = [{ app_id = "${terminalRegexp}"; }];
              "workspace 3" = [{ app_id = "${firefoxRegexp}"; }];
              "workspace 4" = [{ class = "${discordRegexp}"; }];
              "workspace 5" = [
                { app_id = "${spotifyRegexp}"; }
                { class = "${freetubeRegexp}"; }
                { app_id = "${mpvRegexp}"; }
                { class = "${vlcRegexp}"; }
              ];
              "workspace 6" = [
                { class = "${steamRegexp}"; }
                { class = "${epicGamesRegexp}"; }
                { class = "${itchioRegexp}"; }
                { app_id = "${lutrisRegexp}"; }
                { class = "${minecraftRegexp}"; }
              ];
              "workspace 7" = [
                { class = "${steamGameRegexp}"; }
                {
                  class = "${gameRegexp}";
                  app_id = "${gameRegexp}";
                }
              ];
              "workspace 8" = [ ];
              "workspace 9" = [{ class = "${intellijRegexp}"; }];
              "workspace 10" = [ ];
            };

            output = {
              LVDS-1 = {
                scale = "1";
                res = "1366x768";
                bg = "${wallpapersDir}/Gurren Lagann/king_kittan.jpg fill";
                pos = "288 1080";
              };

              DP-1 = {
                scale = "1";
                res = "1920x1080@144Hz";
                pos = "0 0";
                bg = "${wallpapersDir}/Gurren Lagann/simon.jpg fill";
              };

              HDMI-A-1 = {
                scale = "1";
                res = "1920x1080@75Hz";
                bg = "${wallpapersDir}/Jujutsu Kaisen/vol4.jpg fill";
                pos = "1920 190";
              };
            };

            input = {
              "type:keyboard" = {
                xkb_layout = "gb";
                xkb_options = "ctrl:nocaps";
              };

              "12625:16387:ROYUAN_Akko_keyboard" = { xkb_layout = "us"; };

              "2:7:SynPS/2_Synaptics_TouchPad" = {
                dwt = "enabled";
                tap = "enabled";
                natural_scroll = "enabled";
                middle_emulation = "enabled";
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

            window = {
              border = 1;

              commands = [
                {
                  criteria = { window_role = "pop-up"; };
                  command = "floating enable";
                }
                {
                  criteria = { window_role = "task_dialog"; };
                  command = "floating enable";
                }
                {
                  criteria = {
                    title = "${any}";
                    app_id = "${any}";
                    instance = "${any}";
                    class = "${any}";

                  };
                  command = ''
                    {
                      inhibit_idle fullscreen
                      title_format "<b>%title</b> (%app_id%instance,%shell)"
                    }
                  '';
                }
                {
                  criteria = { class = "${steamGameRegexp}"; };
                  command = ''
                    {
                      inhibit_idle fullscreen
                      fullscreen enable
                    }
                  '';
                }
                {
                  criteria = { app_id = "${volumeRegexp}"; };
                  command = ''
                    {
                      floating enable
                      move to scratchpad
                      resize set {
                          width 800
                          height 600
                      }  
                    }
                  '';
                }
                {
                  criteria = { app_id = "${xwvbRegexp}"; };
                  command = ''
                    {
                      floating enable
                      move to scratchpad
                    }
                  '';
                }
                {
                  criteria = { app_id = "${zathuraRegexp}"; };
                  command = ''
                    {
                      floating enable
                      move to scratchpad
                      resize set {
                          width 800
                          height 600
                      }
                    }
                  '';
                }
              ];
            };

            colors = {
              inherit (colours) background;
              focused = {
                inherit (colours) background text;
                border = "${colours.pink}";
                indicator = "${colours.rosewater}";
                childBorder = "${colours.pink}";
              };

              focusedInactive = {
                inherit (colours) background text;
                border = "${colours.mauve}";
                indicator = "${colours.rosewater}";
                childBorder = "${colours.mauve}";
              };

              unfocused = {
                inherit (colours) background text;
                border = "${colours.mauve}";
                indicator = "${colours.rosewater}";
                childBorder = "${colours.mauve}";
              };

              urgent = {
                inherit (colours) background;
                border = "${colours.peach}";
                text = "${colours.peach}";
                indicator = "${colours.overlay0}";
                childBorder = "${colours.peach}";
              };

              placeholder = {
                inherit (colours) background text;
                border = "${colours.overlay0}";
                indicator = "${colours.overlay0}";
                childBorder = "${colours.overlay0}";
              };
            };

            keybindings = {
              "${modifier}+${down}" = "focus down";
              "${modifier}+${left}" = "focus left";
              "${modifier}+${right}" = "focus right";
              "${modifier}+${up}" = "focus up";
              "${modifier}+0" = "workspace number 10";
              "${modifier}+1" = "workspace number 1";
              "${modifier}+2" = "workspace number 2";
              "${modifier}+3" = "workspace number 3";
              "${modifier}+4" = "workspace number 4";
              "${modifier}+5" = "workspace number 5";
              "${modifier}+6" = "workspace number 6";
              "${modifier}+7" = "workspace number 7";
              "${modifier}+8" = "workspace number 8";
              "${modifier}+9" = "workspace number 9";
              "${modifier}+Down" = "focus down";
              "${modifier}+Escape" = ''
                exec "${pkgs.procps}/bin/pkill fuzzel || ${powermenu}/bin/powermenu"'';
              "${modifier}+Left" = "focus left";
              "${modifier}+Minus" = "scratchpad show";
              "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
              "${modifier}+Right" = "focus right";
              "${modifier}+Shift+${down}" = "move down";
              "${modifier}+Shift+${left}" = "move left";
              "${modifier}+Shift+${right}" = "move right";
              "${modifier}+Shift+${up}" = "move up";
              "${modifier}+Shift+0" =
                "move container to workspace number 10, workspace number 10";
              "${modifier}+Shift+1" =
                "move container to workspace number 1, workspace number 1";
              "${modifier}+Shift+2" =
                "move container to workspace number 2, workspace number 2";
              "${modifier}+Shift+3" =
                "move container to workspace number 3, workspace number 3";
              "${modifier}+Shift+4" =
                "move container to workspace number 4, workspace number 4";
              "${modifier}+Shift+5" =
                "move container to workspace number 5, workspace number 5";
              "${modifier}+Shift+6" =
                "move container to workspace number 6, workspace number 6";
              "${modifier}+Shift+7" =
                "move container to workspace number 7, workspace number 7";
              "${modifier}+Shift+8" =
                "move container to workspace number 8, workspace number 8";
              "${modifier}+Shift+9" =
                "move container to workspace number 9, workspace number 9";
              "${modifier}+Shift+o" = "move workspace to output HDMI-A-1";
              "${modifier}+Shift+p" = "move workspace to output LVDS-1";
              "${modifier}+Shift+Down" = "move down";
              "${modifier}+Shift+Left" = "move left";
              "${modifier}+Shift+Right" = "move right";
              "${modifier}+Shift+Up" = "move up";
              "${modifier}+Shift+b" = "border toggle";
              "${modifier}+Shift+c" = "reload";
              "${modifier}+Shift+g" =
                ''exec ${pkgs.emacs}/bin/emacsclient -c -a=""'';
              "${modifier}+Shift+minus" = "move scratchpad";
              "${modifier}+Shift+q" = "kill";
              "${modifier}+Shift+r" = ''mode "resize"'';
              "${modifier}+Shift+space" = "floating toggle";
              "${modifier}+Tab" =
                "${pkgs.procps}/bin/pkill fuzzel || ${windows}/bin/windows";
              "${modifier}+Up" = "focus up";
              "${modifier}+a" = "focus parent";
              "${modifier}+b" = "splith";
              "${modifier}+c" = "exec ${toggleSinkScript}/bin/toggle-sink";
              "${modifier}+d" = ''
                exec "${pkgs.procps}/bin/pkill fuzzel || ${pkgs.fuzzel}/bin/fuzzel"'';
              "${modifier}+e" = "layout toggle split";
              "${modifier}+f" = "fullscreen";
              "${modifier}+r" = ''
                exec "${pkgs.procps}/bin/pkill fuzzel || ${launcher}/bin/launcher"'';
              "${modifier}+s" = "layout stacking";
              "${modifier}+space" = "focus mode_toggle";
              "${modifier}+t" =
                "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
              "${modifier}+v" = "splitv";
              "${modifier}+w" = "layout tabbed";
              "Print" = "exec ${screenshot}";
            };

            startup = [
              { command = "${pkgs.pavucontrol}/bin/pavucontrol"; }
              { command = "${pkgs.swayidle}/bin/swayidle -w"; }
              {
                command =
                  "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch cliphist store";
              }
              {
                command =
                  "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch cliphist store";
              }
              {
                command =
                  "${pkgs.autotiling}/bin/autotiling -w 1 2 3 4 5 6 7 8 9 10";
              }
              { command = "${pkgs.emacs}/bin/emacsclient -c -a=''"; }
              { command = "${pkgs.alacritty}/bin/alacritty"; }
              { command = "${pkgs.zathura}/bin/zathura"; }
              { command = "${pkgs.lutris}/bin/lutris"; }
              {
                command = "${pkgs.procps}/bin/pkill fuzzel";
                always = true;
              }
              {
                command = "systemctl --user restart waybar";
                always = true;
              }
              {
                command = "systemctl --user restart wob";
                always = true;
              }
              {
                command = "systemctl --user restart swaync";
                always = true;
              }
              {
                command =
                  "${pkgs.swaynotificationcenter}/bin/swaync-client -R -rs -sw";
                always = true;
              }
            ];
          };
        };
      };
    };
  };
}
