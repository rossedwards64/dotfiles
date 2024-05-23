{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.sway;
  font = "Iosevka NF";
  mod = "Mod4";
  down = "j";
  right = "l";
  left = "h";
  up = "k";
  wallpapersDir = "/home/ross/Pictures/wallpapers";

  checkMuteScript = import ../scripts/check-mute.nix { inherit pkgs; };
  toggleSinkScript = import ../scripts/toggle-sink.nix { inherit pkgs; };
  wobScript = import ../scripts/wob.nix { inherit pkgs; };

  powermenuScript =
    import ../wm-programs/fuzzel/scripts/powermenu.nix { inherit pkgs; };
  screenshotScript =
    import ../wm-programs/fuzzel/scripts/screenshot.nix { inherit pkgs; };
  windowsScript =
    import ../wm-programs/fuzzel/scripts/windows.nix { inherit pkgs; };

  addToScratchpad = (width: height: ''
    {
        floating enable
        move to scratchpad
        resize set {
            width ${toString width}
            height ${toString height}
        }
    }
  '');

  focusOnGameCommand = ''
    {
      inhibit_idle fullscreen
      fullscreen enable
      focus
    }
  '';

  any = ".*";
  discordRegexp = "^ArmCord$";
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
  zathuraRegexp = "^org.pwmt.zathura$";
  gameRegexp = "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";
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
            inherit down right left up;
            modifier = mod;
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
                bg = "${wallpapersDir}/Gurren Lagann/lordgenome.jpeg fill";
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
              "1:1:AT_Translated_Set_2_keyboard" = {
                xkb_layout = "gb";
                xkb_options = "ctrl:nocaps";
              };

              "12625:16387:ROYUAN_Akko_keyboard" = {
                xkb_layout = "us";
                xkb_options = "ctrl:nocaps,altwin:swap_lalt_lwin";
              };

              "2:7:SynPS/2_Synaptics_TouchPad" = {
                dwt = "enabled";
                tap = "enabled";
                natural_scroll = "enabled";
                middle_emulation = "enabled";
              };
            };

            floating = {
              modifier = mod;
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

                (attrsets.mergeAttrsList (builtins.concatMap (class: [
                  {
                    criteria = { class = "${class}"; };
                    command = focusOnGameCommand;
                  }
                  {
                    criteria = { app_id = "${class}"; };
                    command = focusOnGameCommand;
                  }
                ]) [ "${steamGameRegexp}" "${gameRegexp}" ]))

                (attrsets.mergeAttrsList (builtins.map
                  ({ class, width, height }: {
                    criteria = { class = "${class}"; };
                    command = addToScratchpad width height;
                  }) [
                    {
                      class = "${volumeRegexp}";
                      width = 800;
                      height = 600;
                    }
                    {
                      class = "${zathuraRegexp}";
                      width = 800;
                      height = 600;
                    }
                  ]))
              ];
            };

            keybindings = {
              "${mod}+${down}" = "focus down";
              "${mod}+${left}" = "focus left";
              "${mod}+${right}" = "focus right";
              "${mod}+${up}" = "focus up";
              "${mod}+Down" = "focus down";
              "${mod}+Escape" = ''
                exec "${pkgs.procps}/bin/pkill fuzzel || ${powermenuScript}/bin/powermenu"'';
              "${mod}+Left" = "focus left";
              "${mod}+Minus" = "scratchpad show";
              "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
              "${mod}+Right" = "focus right";
              "${mod}+Shift+${down}" = "move down";
              "${mod}+Shift+${left}" = "move left";
              "${mod}+Shift+${right}" = "move right";
              "${mod}+Shift+${up}" = "move up";
              "${mod}+Shift+o" = "move workspace to output HDMI-A-1";
              "${mod}+Shift+p" = "move workspace to output DP-1 ";
              "${mod}+Shift+Down" = "move down";
              "${mod}+Shift+Left" = "move left";
              "${mod}+Shift+Right" = "move right";
              "${mod}+Shift+Up" = "move up";
              "${mod}+Shift+b" = "border toggle";
              "${mod}+Shift+c" = "reload";
              "${mod}+Shift+g" =
                ''exec ${pkgs.emacs}/bin/emacsclient -c -a=""'';
              "${mod}+Shift+minus" = "move scratchpad";
              "${mod}+Shift+q" = "kill";
              "${mod}+Shift+r" = ''mode "resize"'';
              "${mod}+Shift+space" = "floating toggle";
              "${mod}+Tab" =
                "${pkgs.procps}/bin/pkill fuzzel || ${windowsScript}/bin/windows";
              "${mod}+Up" = "focus up";
              "${mod}+a" = "focus parent";
              "${mod}+b" = "splith";
              "${mod}+c" = "exec ${toggleSinkScript}/bin/toggle-sink";
              "${mod}+d" = ''
                exec "${pkgs.procps}/bin/pkill fuzzel || ${pkgs.fuzzel}/bin/fuzzel"'';
              "${mod}+e" = "layout toggle split";
              "${mod}+f" = "fullscreen";
              "${mod}+s" = "layout stacking";
              "${mod}+space" = "focus mode_toggle";
              "${mod}+t" =
                "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
              "${mod}+v" = "splitv";
              "${mod}+w" = "layout tabbed";
              "Print" = "exec ${screenshotScript}/bin/screenshot";
            } // (attrsets.mergeAttrsList (builtins.map (num: {
              "${mod}+${toString num}" = "workspace number ${toString num}";
              "${mod}+Shift+${toString num}" =
                "move container to workspace number ${
                  toString num
                }, workspace number ${toString num}";
            }) (lists.range 0 9)));

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
