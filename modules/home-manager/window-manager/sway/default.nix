{ lib, config, pkgs, ... }:
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
  session_vars = ''
    DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP \
                      XDG_SESSION_TYPE XDG_SESSION_DESKTOP SWAYSOCK'';
  config_home = "$HOME/.config";
  data_home = "$HOME/.local";
  lockscript = "$data_home/bin/lock";
  menu = "$rofiscripts/launcher";
  mutescript = "$data_home/bin/check-mute";
  powermenu = "$ROFISCRIPTS/powermenu";
  rofiscripts = "$config_home/rofi/scripts";
  runner = "$rofiscripts/runner";
  screenshot = "$rofiscripts/screenshot";
  windows = "$rofiscripts/windows";
  wobscript = "$data_home/bin/wob";
  wobsock = "$XDG_RUNTIME_DIR/wob.sock";
  any = ".*";
  discord = "^discord$";
  emacs = "^emacs(client)?$";
  epicgames = "^heroic$";
  factorio = "^factorio$";
  firefox = "^firefox$";
  freetube = "^FreeTube$";
  game = "^(factorio|youronlymoveishustle|dwarfort|gamescope).*$";
  intellij = "^jetbrains-idea$";
  itchio = "^itch$";
  lutris = "^lutris$";
  minecraft = "^com-atlauncher-App$";
  mpv = "^mpv$";
  spotify = "^(dev.alextren.)?Spot(ify)?$";
  steam = "^steam$";
  steam_game = "^steam_app_[0-9]*$";
  swaync = "^swaync(-client)?$";
  terminal = "^Alacritty$";
  vlc = "^vlc$";
  volume = "^pavucontrol$";
  xdman = "^xdm-app$";
  xwvb = "^xwaylandvideobridge$";
  yomihustle = "^youronlymoveishustle.*$";
  zathura = "^org.pwmt.zathura$";
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
    home.packages = with pkgs; [ sway ];

    wayland = {
      windowManager = {
        sway = {
          enable = true;
          xwayland = true;

          systemd = {
            enable = true;
            xdgAutostart = true;
          };

          swaynag = { enable = true; };

          extraConfigEarly = "";
          extraConfig = "";
          extraSessionCommands = "";
          extraOptions = [ ];

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

            output = {
              LVDS-1 = {
                scale = 1;
                res = "1366x768";
                bg = "/home/ross/Pictures/wallpapers/lordgenome.png fill";
                pos = "288 1080";
              };

              DP-1 = {
                scale = 1;
                res = "1920x1080@144Hz";
                pos = "0 0";
                bg =
                  "/home/ross/Pictures/wallpapers/Gurren Lagann/simon.jpg fill";
              };

              HDMI-A-1 = {
                scale = 1;
                res = "1920x1080@75Hz";
                bg = "/home/ross/Pictures/wallpapers/gojo.png fill";
                pos = "0 0";
              };
            };

            window = {
              border = 1;

              commands = [
                {
                  criteria = {
                    window_role = "pop-up";
                    floating = true;
                  };
                }
                {
                  criteria = {
                    window_role = "task_dialog";
                    floating = true;
                  };
                }
                {
                  criteria = {
                    title = "${any}";
                    app_id = "${any}";
                    instance = "${any}";
                    class = "${any}";
                    inhibit_idle = "fullscreen";
                    title_format = "<b>%title</b> (%app_id%instance,%shell)";
                  };
                }
                {
                  criteria = {
                    class = "${steam_game}";
                    inhibit_idle = "fullscreen";
                    fullscreen = true;
                  };
                }
                {
                  criteria = {
                    app_id = "${volume}";
                    floating = true;
                  };
                  command = "move to scratchpad";
                }
                {
                  criteria = {
                    app_id = "${xdman}";
                    urgent = true;
                  };
                  command = "move to scratchpad";
                }
                {
                  criteria = {
                    app_id = "${xwvb}";
                    floating = true;
                  };
                  command = "move to scratchpad";
                }
                {
                  criteria = {
                    app_id = "${zathura}";
                    floating = true;
                  };
                  command = "move to scratchpad";
                }
              ];
            };

            floating = {
              inherit modifier;
              border = 1;
              criteria = [ ];
              titlebar = true;
            };

            focus = {
              followMouse = true;
              mouseWarping = true;
              newWindow = "urgent";
            };

            #seat = {
            #  "seat0" = { xcursor_theme = "Catppuccin-Mocha-Dark-Cursors 24"; };
            #};

            colors = {
              background = "${background}";
              focused = {
                inherit (colours) background text;
                border = "${pink}";
                indicator = "${rosewater}";
                childBorder = "${pink}";
              };

              focusedInactive = {
                inherit (colours) background text;
                border = "${mauve}";
                indicator = "${rosewater}";
                childBorder = "${mauve}";
              };

              unfocused = {
                inherit (colours) background text;
                border = "${mauve}";
                indicator = "${rosewater}";
                childBorder = "${mauve}";
              };

              urgent = {
                inherit (colours) background;
                border = "${peach}";
                text = "${peach}";
                indicator = "${overlay0}";
                childBorder = "${peach}";
              };

              placeholder = {
                inherit (colours) background text;
                border = "${overlay0}";
                indicator = "${overlay0}";
                childBorder = "${overlay0}";
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
              smartGaps = true;
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
              "${modifier}+Ctrl+g" =
                ''exec emacsclient --eval "(emacs-everywhere)"'';
              "${modifier}+Down" = "focus down";
              "${modifier}+Escape" = ''exec "pkill rofi || ${powermenu}"'';
              "${modifier}+Left" = "focus left";
              "${modifier}+Minus" = "scratchpad show";
              "${modifier}+Return" = "exec alacritty";
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
              "${modifier}+Shift+g" = ''exec emacsclient -c -a=""'';
              "${modifier}+Shift+minus" = "move scratchpad";
              "${modifier}+Shift+q" = "kill";
              "${modifier}+Shift+r" = ''mode "resize"'';
              "${modifier}+Shift+s" = "exec ${lock}";
              "${modifier}+Shift+space" = "floating toggle";
              "${modifier}+Tab" = "pkill rofi || ${windows}";
              "${modifier}+Up" = "focus up";
              "${modifier}+a" = "focus parent";
              "${modifier}+b" = "splith";
              "${modifier}+d" = ''exec "pkill rofi || ${menu}"'';
              "${modifier}+e" = "layout toggle split";
              "${modifier}+f" = "fullscreen";
              "${modifier}+r" = ''exec "pkill rofi || ${runner}"'';
              "${modifier}+s" = "layout stacking";
              "${modifier}+space" = "focus mode_toggle";
              "${modifier}+t" = "exec swaync-client -t -sw";
              "${modifier}+v" = "splitv";
              "${modifier}+w" = "layout tabbed";
              "Print" = "exec ${screenshot}";
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

            startup = {
              pavucontrol.command = "pavucontrol";
              swayidle.command = "swayidle -w";
              udiskie.command = "udiske -ant";
              pasteimage.command =
                "wl-paste --type image --watch cliphist store";
              pastetext.command = "wl-paste --type text --watch cliphist store";
              autotiling.command = "autotiling -w 1 2 3 4 5 6 7 8 9 10";
              emacs.command = "emacsclient -c -a=''";
              alacritty.command = "alacritty";
              zathura.command = "zathura";
              discord.command = "discord";
              steam.command = "steam";
              spotify.command = "flatpak run com.Spotify.Client";
              lutris.command = "lutris";
              obs.command = "obs --minimize-to-tray";

              rofi = {
                command = "pkill rofi";
                always = true;
              };

              waybar = {
                command = "systemctl --user restart waybar";
                always = true;
              };

              wobsock = {
                command =
                  "rm -f ${wobsock} && mkfifo ${wobsock} && tail -f ${wobsock} | wob";
                always = true;
              };

              swaync = {
                command = "swaync-client -R -rs -sw";
                always = true;
              };
            };
          };
        };
      };
    };
  };
}
