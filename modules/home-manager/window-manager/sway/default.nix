{
  lib,
  config,
  pkgs,
  ...
}:
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

  powermenuScript = import ../wm-programs/fuzzel/scripts/powermenu.nix { inherit pkgs; };
  screenshotScript = import ../wm-programs/fuzzel/scripts/screenshot.nix { inherit pkgs; };
  windowsScript = import ../wm-programs/fuzzel/scripts/windows.nix { inherit pkgs; };

  addToScratchpad = (
    width: height: ''
      {
          floating enable
          move to scratchpad
          resize set {
              width ${toString width}
              height ${toString height}
          }
      }
    ''
  );

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
  gameConquerorRegexp = "^GameConqueror.py$";
  gameRegexp = "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";
  intellijRegexp = "^jetbrains-idea$";
  itchioRegexp = "^itch$";
  lutrisRegexp = "^lutris$";
  minecraftRegexp = "^com-atlauncher-App$";
  mpvRegexp = "^mpv$";
  qBitTorrentRegexp = "^org.qbittorrent.qBittorrent$";
  spotifyRegexp = "^(dev.alextren.)?Spot(ify)?$";
  steamGameRegexp = "^steam_app_[0-9]*$";
  steamRegexp = "^steam$";
  terminalRegexp = "^Alacritty$";
  vlcRegexp = "^vlc$";
  volumeRegexp = "^pavucontrol$";
  zathuraRegexp = "^org.pwmt.zathura$";
in
{
  options.modules.sway = {
    enable = mkEnableOption "sway";
  };

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
            variables = [ "--all" ];
          };

          swaynag = {
            enable = true;
          };

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
            inherit
              modifier
              down
              right
              left
              up
              ;
            bindkeysToCode = false;
            defaultWorkspace = "workspace number 1";
            menu = "${pkgs.fuzzel}/bin/fuzzel";
            terminal = "${pkgs.alacritty}/bin/alacritty";
            workspaceAutoBackAndForth = true;
            workspaceLayout = "tabbed";
            workspaceOutputAssign = [ ];
            bars = [ ];

            seat = {
              "seat0" = {
                xcursor_theme = "Catppuccin-Mocha-Dark-Cursors 24";
              };
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
              smartGaps = false;
            };

            assigns = {
              "workspace 1" = [ { app_id = emacsRegexp; } ];
              "workspace 2" = [ { app_id = terminalRegexp; } ];
              "workspace 3" = [ { app_id = firefoxRegexp; } ];
              "workspace 4" = [ { class = discordRegexp; } ];
              "workspace 5" = [
                { app_id = spotifyRegexp; }
                { class = freetubeRegexp; }
                { app_id = mpvRegexp; }
                { class = vlcRegexp; }
              ];
              "workspace 6" = [
                { class = steamRegexp; }
                { class = epicGamesRegexp; }
                { class = itchioRegexp; }
                { app_id = lutrisRegexp; }
                { class = minecraftRegexp; }
                { app_id = gameConquerorRegexp; }
              ];
              "workspace 7" = [
                { class = steamGameRegexp; }
                { class = gameRegexp; }
                { app_id = gameRegexp; }
              ];
              "workspace 8" = [ ];
              "workspace 9" = [ { class = intellijRegexp; } ];
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
              modifier = modifier;
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

              commands =
                [
                  {
                    criteria = {
                      window_role = "pop-up";
                    };
                    command = "floating enable";
                  }
                  {
                    criteria = {
                      window_role = "task_dialog";
                    };
                    command = "floating enable";
                  }
                  {
                    criteria = {
                      title = any;
                      app_id = any;
                      instance = any;
                      class = any;

                    };
                    command = ''
                      {
                        inhibit_idle fullscreen
                        title_format "<b>%title</b> (%app_id%instance,%shell)"
                      }
                    '';
                  }
                ]
                ++ (builtins.concatMap
                  (class: [
                    {
                      criteria = {
                        class = class;
                      };
                      command = focusOnGameCommand;
                    }
                    {
                      criteria = {
                        app_id = class;
                      };
                      command = focusOnGameCommand;
                    }
                  ])
                  [
                    steamGameRegexp
                    gameRegexp
                  ]
                )
                ++ (builtins.map
                  (
                    {
                      app_id,
                      width,
                      height,
                    }:
                    {
                      criteria = {
                        inherit app_id;
                      };
                      command = addToScratchpad width height;
                    }
                  )
                  [
                    {
                      app_id = volumeRegexp;
                      width = 800;
                      height = 600;
                    }
                    {
                      app_id = zathuraRegexp;
                      width = 800;
                      height = 600;
                    }
                    {
                      app_id = qBitTorrentRegexp;
                      width = 800;
                      height = 600;
                    }
                  ]
                );
            };

            keybindings = attrsets.mergeAttrsList [
              (attrsets.mergeAttrsList (
                lib.flatten [
                  (builtins.map (num: {
                    "${modifier}+${toString num}" = "workspace number ${toString num}";
                    "${modifier}+Ctrl+${toString num}" = "move container to workspace number ${toString num}";
                    "${modifier}+Shift+${toString num}" = "move container to workspace number ${toString num}, workspace number ${toString num}";
                  }) (lists.range 0 9))
                  (builtins.map
                    (direction: {
                      "${modifier}+${direction}" = "focus ${direction}";
                      "${modifier}+Shift+${direction}" = "move ${direction}";
                    })
                    [
                      left
                      right
                      up
                      down
                    ]
                  )
                ]
              ))
              (attrsets.concatMapAttrs
                (key: monitor: {
                  "${modifier}+${key}" = "move workspace to output ${monitor}";
                })
                {
                  o = "HDMI-A-1";
                  p = "DP-1";
                }
              )
              {
                "${modifier}+Escape" = ''exec "${pkgs.procps}/bin/pkill fuzzel || ${powermenuScript}/bin/powermenu"'';
                "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
                "${modifier}+Shift+Return" = "exec ${pkgs.firefox}/bin/firefox";
                "${modifier}+Shift+g" = "exec ${pkgs.emacs29-pgtk}/bin/emacsclient -c -a=''";
                "${modifier}+Tab" = "${pkgs.procps}/bin/pkill fuzzel || ${windowsScript}/bin/windows";
                "${modifier}+c" = "exec ${toggleSinkScript}/bin/toggle-sink";
                "${modifier}+d" = ''exec "${pkgs.procps}/bin/pkill fuzzel || ${pkgs.fuzzel}/bin/fuzzel"'';
                "${modifier}+t" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
                "Print" = "exec ${screenshotScript}/bin/screenshot";
                "${modifier}+g" = "splith";
                "${modifier}+v" = "splitv";
                "${modifier}+e" = "layout toggle split";
                "${modifier}+s" = "layout stacking";
                "${modifier}+w" = "layout tabbed";
                "${modifier}+f" = "fullscreen";
                "${modifier}+Shift+c" = "reload";
                "${modifier}+Shift+q" = "kill";
                "${modifier}+r" = ''mode "default"'';
                "${modifier}+Shift+r" = ''mode "resize"'';
                "${modifier}+Minus" = "scratchpad show";
                "${modifier}+Shift+minus" = "move scratchpad";
                "${modifier}+Shift+b" = "border toggle";
                "${modifier}+Shift+space" = "floating toggle";
                "${modifier}+a" = "focus parent";
                "${modifier}+space" = "focus mode_toggle";
              }
            ];

            startup = [
              {
                command = "${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start --foreground --components=pkcs11,secrets,ssh";
              }
              { command = "${pkgs.pavucontrol}/bin/pavucontrol"; }
              { command = "${pkgs.swayidle}/bin/swayidle -w"; }
              {
                command = "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch cliphist store";
              }
              {
                command = "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch cliphist store";
              }
              {
                command = "${pkgs.autotiling}/bin/autotiling -w 1 2 3 4 5 6 7 8 9 10";
              }
              { command = "${pkgs.alacritty}/bin/alacritty"; }
              { command = "${pkgs.emacs29-pgtk}/bin/emacsclient -c -a=''"; }
              { command = "${pkgs.firefox}/bin/firefox"; }
              { command = "${pkgs.lutris}/bin/lutris"; }
              { command = "${pkgs.qbittorrent}/bin/qbittorrent"; }
              { command = "${pkgs.spot}/bin/spot"; }
              { command = "${pkgs.zathura}/bin/zathura"; }
              { command = "${pkgs.steam}/bin/steam"; }
              { command = "${pkgs.armcord}/bin/armcord"; }
              {
                command = "${pkgs.flatpak}/bin/flatpak run io.freetubeapp.FreeTube";
              }
              {
                command = "${pkgs.procps}/bin/pkill fuzzel";
                always = true;
              }
              {
                command = "${pkgs.systemd}/bin/systemctl --user restart waybar";
                always = true;
              }
              {
                command = "${pkgs.systemd}/bin/systemctl --user restart wob";
                always = true;
              }
              {
                command = "${pkgs.systemd}/bin/systemctl --user restart swaync";
                always = true;
              }
              {
                command = "${pkgs.swaynotificationcenter}/bin/swaync-client -R -rs -sw";
                always = true;
              }
            ];
          };
        };
      };
    };
  };
}
