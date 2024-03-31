{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.hyprland;
  mod = "SUPER";
  home = "/home/ross";
  font = "Iosevka NF";
  monitor1 = "DP-1";
  monitor2 = "HDMI-A-1";
  text_color = "0xffffffff";
  wobscript = "${home}/.local/bin/wob";
  screenshot = "${home}/.config/rofi/scripts/screenshot";
  menu = "${home}/.config/rofi/scripts/launcher";
  runner = "${home}/.config/rofi/scripts/runner";
  powermenu = "${home}/.config/rofi/scripts/powermenu";
  clipboard = "${home}/.config/rofi/scripts/clipboard.sh";
  reload = ''hyprctl reload && notify-send "Reloaded Hyprland"'';
  gameTabs = ''hyprctl --batch "workspace 7 ; togglegroup"'';
  wobsock = "$XDG_RUNTIME_DIR/wob.sock";
  any = "^(.*)$";
  steamGame = "^(steam_app_[0-9]*)$";
  discord = "^(discord)$";
  emacs = "^(emacs(client)?)$";
  epicGames = "^(heroic)$";
  firefox = "^(firefox)$";
  freetube = "^(FreeTube)$";
  intellij = "^(jetbrains-idea)$";
  itchio = "^(itch)$";
  lutris = "^(lutris)$";
  minecraft = "^(com-atlauncher-App)$";
  mpv = "^(mpv)$";
  spotify = "^(Spotify)$";
  steam = "^(steam)$";
  terminal = "^(Alacritty)$";
  vlc = "^(vlc)$";
  xwvb = "^(xwaylandvideobridge)$";
  xdman = "^(xdm-app)$";
  game = "^(factorio|youronlymoveishustle|dwarfort|gamescope)$";
  virtManager = "^(virt-manager)$";
  zathura = "^(org.pwmt.zathura)$";
  volume = "^(pavucontrol)$";
  activeCol = "rgb(64727d)";
  inactiveCol = "rgb(212b30)";
  activeGroup = "rgb(1794d2)";
  inactiveGroup = "${inactiveCol}";
  groupActiveCol = "0x66ffff00";
  groupInactiveCol = "0x66777700";
  groupLockedActiveCol = "0x66ff5500";
  groupLockedInactiveCol = "0x66775500";
  groupCols = {
    active = groupActiveCol;
    inactive = groupInactiveCol;
    locked_active = groupLockedActiveCol;
    locked_inactive = groupLockedInactiveCol;
  };
in {
  options.modules.hyprland = { enable = mkEnableOption "hyprland"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ];

    wayland = {
      windowManager = {
        hyprland = {
          enable = true;
          plugins = [ ];

          settings = {

            env = [
              "XCURSOR,24"
              "XDG_CURRENT_DESKTOP,Hyprland"
              "XDG_SESSION_TYPE,wayland"
              "XDG_SESSION_DESKTOP,Hyprland"
            ];

            monitor = [
              "${monitor1},1920x1080@144,0x0,1"
              "${monitor2},1920x1080@75,1920x190,1"
            ];

            exec-once = [
              "hyprpaper"
              "wl-paste --type text --watch cliphist store"
              "wl-paste --type image --watch cliphist store"
              "udiskie -ant &"
              "obs --minimize-to-tray"
              "pavucontrol"
              "emacsclient -c -a=''"
              "alacritty"
              "firefox"
              "flatpak run com.discordapp.Discord"
              "flatpak run com.spotify.Client"
              "steam"
              "flatpak run io.freetube.FreeTube"
              # "virt-manager"
            ];

            exec = [ "systemctl --user restart waybar" ];

            general = {
              gaps_in = 5;
              gaps_out = 5;
              layout = "master";
              apply_sens_to_raw = true;
              resize_on_border = true;
              hover_icon_on_border = true;
              extend_border_grab_area = 5;
              border_size = 2;
              "col.active_border" = activeCol;
              "col.inactive_border" = inactiveCol;
            };

            input = {
              kb_file = "";
              kb_layout = "us";
              kb_variant = "";
              kb_model = "";
              kb_options = "ctrl:nocaps";
              kb_rules = "";
              follow_mouse = true;
              sensitivity = 0;
            };

            decoration = {
              blur = {
                enabled = true;
                size = 3;
                passes = 3;
                new_optimizations = true;
                xray = true;
                ignore_opacity = true;
                noise = 0;
                contrast = 0.9;
                brightness = 0.9;
                special = true;
              };

              rounding = 15;
              active_opacity = 0.9;
              inactive_opacity = 0.8;
              fullscreen_opacity = 1.0;
              shadow_range = 10;
              shadow_range_power = 3;
              "col.shadow" = "0x990f0f0f";
            };

            binds = {
              workspace_back_and_forth = true;
              allow_workspace_cycles = true;
            };

            dwindle = {
              pseudotile = true;
              preserve_split = true;
              force_split = true;
              use_active_for_split = true;
            };

            master = {
              new_is_master = false;
              new_on_top = true;
              no_gaps_when_only = false;
              orientation = "left";
              inherit_fullscreen = true;
              always_center_master = false;
            };

            group = {
              groupbar = {
                font_size = 10;
                gradients = false;
                render_titles = true;
                scrolling = true;
                text_color = "0xffffffff";
                "col.active" = groupCols.active;
                "col.inactive" = groupCols.inactive;
                "col.locked_active" = groupCols.locked_active;
                "col.locked_inactive" = groupCols.locked_inactive;
              };

              insert_after_current = true;
              focus_removed_window = true;
              "col.active" = groupCols.active;
              "col.inactive" = groupCols.inactive;
              "col.locked_active" = groupCols.locked_active;
              "col.locked_inactive" = groupCols.locked_inactive;
            };

            misc = {
              vfr = false;
              vrr = 1;
              mouse_move_enables_dpms = true;
              key_press_enables_dpms = true;
              animate_mouse_windowdragging = true;
              animate_manual_resizes = true;
              enable_swallow = true;
              no_direct_scanout = false;
              cursor_zoom_factor = 1;
              force_default_wallpaper = 0;
            };

            animations = {
              enabled = true;
              bezier = [
                "overshot,0.05,0.9,0.1,1.05"
                "smoothOut,0.36,0,0.66,-0.56"
                "smoothIn,0.25,1,0.5,1"
              ];

              animation = [
                "windowsIn,1,5,overshot,popin"
                "windowsOut,1,4,smoothOut,popin"
                "windowsMove,1,4,default,popin"
                "border,1,10,default"
                "borderangle,1,10,default,loop"
                "fade,1,10,smoothIn"
                "fadeDim,1,10,smoothIn"
                "workspaces,1,6,default,slide"
              ];
            };

            windowrulev2 = [
              "noblur,class:${firefox}"
              "opaque,class:${firefox}"
              "noblur,class:${freetube}"
              "opaque,class:${freetube}"
              "noblur,class:${mpv}"
              "opaque,class:${mpv}"
              "noblur,class:${vlc}"
              "opaque,class:${vlc}"
              "noblur,class:${discord}"
              "opaque,class:${discord}"
              "opacity 0.0 override 0.0 override override,class:${xwvb}"
              "noanim,class:${xwvb}"
              "nofocus,class:${xwvb}"
              "noinitialfocus,class:${xwvb}"
              "workspace 1 silent,class:${emacs}"
              "workspace 2 silent,class:${terminal}"
              "workspace 3 silent,class:${firefox}"
              "workspace 4 silent,class:${discord}"
              "workspace 5 silent,class:${spotify}"
              "workspace 6 silent,class:${steam}"
              "workspace 6 silent,class:${lutris}"
              "workspace 6 silent,class:${minecraft}"
              "workspace 6 silent,class:${itchio}"
              "workspace 6 silent,class:${epicGames}"
              "workspace 8 silent,class:${virtManager}"
              "workspace 9 silent,class:${intellij}"
              "workspace special silent,class:${xwvb}"
              "workspace special silent,class:${volume}"
              "workspace special silent,class:${xdman}"
              "workspace special silent,class:${zathura}"
              "group new,class:${firefox}"
              "group new,class:${steam}"
              "workspace 7,class:${steamGame}"
              "monitor ${monitor1},class:${steamGame}"
              "noblur,class:${steamGame}"
              "opaque,class:${steamGame}"
              "center,class:${steamGame}"
              "fullscreen,class:${steamGame}"
              "idleinhibit always,class:${steamGame}"
              "workspace 7,class:${game}"
              "monitor ${monitor1},class:${game}"
              "noblur,class:${game}"
              "opaque,class:${game}"
              "center,class:${game}"
              "fullscreen,class:${game}"
              "idleinhibit always,class:${game}"
            ];

            workspace = [ "7,monitor:${monitor1}" ];

            layerrule = [ "blur,rofi" ];

            bind = [
              "${mod},RETURN,exec,alacritty"
              ''${mod}SHIFT,e,exec,emacsclient -c -a=""''
              "${mod}SHIFT,c,exec,${reload}"
              "${mod},f,fullscreen"
              "${mod}SHIFT,q,killactive,"
              "${mod}SHIFT,m,exit,"
              "${mod},y,togglefloating,"
              "${mod},p,pseudo,"
              "${mod},v,exec,${clipboard}"
              "${mod},t,exec,swaync-client -t"
              "${mod}SHIFT,b,pin"
              ",Print,exec,${screenshot}"
              ''SHIFT,Print,exec,${screenshot} "${home}/Pictures/screenshots"''
              ''${mod},C,exec,"${home}/.local/bin/toggle_sink"''
              "${mod}SHIFT,o,movecurrentworkspacetomonitor,${monitor2}"
              "${mod}SHIFT,p,movecurrentworkspacetomonitor,${monitor1}"
              "${mod},minus,togglespecialworkspace"
              "${mod},1,workspace,1"
              "${mod},2,workspace,2"
              "${mod},3,workspace,3"
              "${mod},4,workspace,4"
              "${mod},5,workspace,5"
              "${mod},6,workspace,6"
              "${mod},7,workspace,7"
              "${mod},8,workspace,8"
              "${mod},9,workspace,9"
              "${mod},0,workspace,10"
              "${mod}SHIFT,minus,movetoworkspace,special"
              "${mod}SHIFT,1,movetoworkspace,1"
              "${mod}SHIFT,2,movetoworkspace,2"
              "${mod}SHIFT,3,movetoworkspace,3"
              "${mod}SHIFT,4,movetoworkspace,4"
              "${mod}SHIFT,5,movetoworkspace,5"
              "${mod}SHIFT,6,movetoworkspace,6"
              "${mod}SHIFT,7,movetoworkspace,7"
              "${mod}SHIFT,9,movetoworkspace,9"
              "${mod}SHIFT,8,movetoworkspace,8"
              "${mod}SHIFT,0,movetoworkspace,10"
              "${mod},mouse_down,workspace,e+1"
              "${mod},mouse_up,workspace,e-1"
              "${mod},g,togglegroup"
              "${mod}SHIFT,g,changegroupactive,f"
              "${mod}SHIFT,f,changegroupactive,g"
            ];

            bindm =
              [ "${mod},mouse:272,movewindow" "${mod},mouse:273,resizewindow" ];

            bindr = [
              "${mod},d,exec,pkill rofi || ${menu}"
              "${mod},r,exec,pkill rofi || ${runner}"
              "${mod},ESCAPE,exec,pkill rofi || ${powermenu}"
            ];

            binde = [
              "${mod},left,movefocus,l"
              "${mod},h,movefocus,l"
              "${mod},right,movefocus,r"
              "${mod},l,movefocus,r"
              "${mod},up,movefocus,u"
              "${mod},k,movefocus,u"
              "${mod},down,movefocus,d"
              "${mod},j,movefocus,d"
              "${mod}SHIFT,h,movewindow,l"
              "${mod}SHIFT,left,movewindow,l"
              "${mod}SHIFT,l,movewindow,r"
              "${mod}SHIFT,right,movewindow,r"
              "${mod}SHIFT,k,movewindow,u"
              "${mod}SHIFT,up,movewindow,u"
              "${mod}SHIFT,j,movewindow,d"
              "${mod}SHIFT,down,movewindow,d"
              "${mod}CTRL,h,resizeactive,-10 0"
              "${mod}CTRL,l,resizeactive,10 0"
              "${mod}CTRL,k,resizeactive,0 -10"
              "${mod}CTRL,j,resizeactive,0 10"
            ];

            bindl = [
              ''
                ,XF86AudioMute,exec,wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && "${home}/.local/bin/check_mute" "-s"''
              ''
                ,XF86AudioMicMute,exec,wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && "${home}/.local/bin/check_mute" "-m"''

            ];

            bindel = [
              ",XF86MonBrightnessUp,exec,brightnessctl set 5%+"
              ",XF86MonBrightnessDown,exec,brightnessctl set 5%-"
              '',XF86AudioRaiseVolume,exec,${wobscript} "-v" "-i5"''
              '',XF86AudioLowerVolume,exec,${wobscript} "-v" "-d5"''
              ",XF86AudioNext,exec,playerctl next"
              ",XF86AudioPrev,exec,playerctl previous"
              ",XF86AudioPlay,exec,playerctl play-pause"
              ",XF86AudioStop,exec,playerctl stop"
            ];
          };
        };
      };
    };
  };
}
