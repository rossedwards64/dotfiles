{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.hyprland;
  mod = "SUPER";
  font = "Iosevka NF";
  monitor1 = "DP-1";
  monitor2 = "HDMI-A-1";
  laptop = "LVDS-1";
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

  reload = ''hyprctl reload && notify-send "Reloaded Hyprland"'';
  gameTabs = ''hyprctl --batch "workspace 7 ; togglegroup"'';

  any = "^(.*)$";
  steamGameRegexp = "^(steam_app_[0-9]*)$";
  discordRegexp = "^(ArmCord)$";
  emacsRegexp = "^(emacs(client)?)$";
  epicGamesRegexp = "^(heroic)$";
  firefoxRegexp = "^(firefox)$";
  freetubeRegexp = "^(FreeTube)$";
  intellijRegexp = "^(jetbrains-idea)$";
  itchioRegexp = "^(itch)$";
  lutrisRegexp = "^(lutris)$";
  minecraftRegexp = "^(com-atlauncher-App)$";
  mpvRegexp = "^(mpv)$";
  spotifyRegexp = "^(Spotify)$";
  steamRegexp = "^(steam)$";
  terminalRegexp = "^(Alacritty)$";
  vlcRegexp = "^(vlc)$";
  gameRegexp = "^(factorio|youronlymoveishustle|dwarfort|gamescope)$";
  virtManagerRegexp = "^(virt-manager)$";
  zathuraRegexp = "^(org.pwmt.zathura)$";
  volumeRegexp = "^(pavucontrol)$";

  setRule = rule: regexp: "${rule},class:${regexp}";

  noComposite = builtins.concatMap (regexp: [
    (setRule "noblur" regexp)
    (setRule "opaque" regexp)
    (setRule "center" regexp)
    (setRule "fullscreen" regexp)
    (setRule "idleinhibit always" regexp)
    (setRule "group new" regexp)
  ]) [ steamGameRegexp gameRegexp ];

  noBlurAndOpaque = builtins.concatMap
    (regexp: [ (setRule "noblur" regexp) (setRule "opaque" regexp) ]) [
      firefoxRegexp
      freetubeRegexp
      mpvRegexp
      vlcRegexp
      discordRegexp
    ];

  workspaceAssigns = builtins.map (assignment:
    ((num: regexp: "workspace ${toString num} silent,class:${regexp}")
      (head assignment) (lists.last assignment))) [
        [ 1 emacsRegexp ]
        [ 2 terminalRegexp ]
        [ 3 firefoxRegexp ]
        [ 4 discordRegexp ]
        [ 5 spotifyRegexp ]
        [ 6 steamRegexp ]
        [ 6 lutrisRegexp ]
        [ 6 minecraftRegexp ]
        [ 6 itchioRegexp ]
        [ 6 epicGamesRegexp ]
        [ 7 steamGameRegexp ]
        [ 7 gameRegexp ]
        [ 8 virtManagerRegexp ]
        [ 9 intellijRegexp ]
        [ "special" volumeRegexp ]
        [ "special" zathuraRegexp ]
      ];

  monitorAssigns = builtins.map (assignment:
    (monitor: regexp: "monitor ${monitor},class:${regexp}") (head assignment)
    (lists.last assignment)) [
      [ monitor1 steamGameRegexp ]
      [ monitor1 gameRegexp ]
    ];
in {
  options.modules.hyprland = { enable = mkEnableOption "hyprland"; };

  config = mkIf cfg.enable {
    xdg.configFile."hypr/hyprpaper.conf".text = ''
      preload = ${wallpapersDir}/Gurren Lagann/simon.jpg
      preload = ${wallpapersDir}/Jujutsu Kaisen/yuji.png
      preload = ${wallpapersDir}/Gurren Lagann/king_kittan.jpg
      wallpaper = ${monitor1},${wallpapersDir}/Gurren Lagann/simon.jpg
      wallpaper = ${monitor2},${wallpapersDir}/Jujutsu Kaisen/yuji.png
      wallpaper = ${laptop},${wallpapersDir}/Gurren Lagann/lordgenome.jpeg
    '';

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
              "${laptop},1366x768@60,0x0,1"
            ];

            exec-once = [
              "${pkgs.swayidle}/bin/swayidle -w"
              "${pkgs.hyprpaper}/bin/hyprpaper"
              "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch cliphist store"
              "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch cliphist store"
              "${pkgs.pavucontrol}/bin/pavucontrol"
              "${pkgs.emacs29-pgtk}/bin/emacsclient -c -a=''"
              "${pkgs.alacritty}/bin/alacritty"
              "${pkgs.firefox}/bin/firefox"
              "${pkgs.lutris}/bin/lutris"
              "${pkgs.steam}/bin/steam"
            ];

            exec = [
              "systemctl --user restart waybar"
              "systemctl --user restart wob"
            ];

            general = {
              gaps_in = 5;
              gaps_out = 5;
              layout = "master";
              apply_sens_to_raw = true;
              resize_on_border = true;
              hover_icon_on_border = true;
              extend_border_grab_area = 5;
              border_size = 2;
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

            device = {
              name = "royuan-akko-keyboard";
              kb_options = "ctrl:nocaps,altwin:swap_lalt_lwin";
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
            };

            binds = {
              workspace_back_and_forth = true;
              allow_workspace_cycles = true;
            };

            dwindle = {
              pseudotile = true;
              preserve_split = true;
              force_split = true;
              use_active_for_splits = true;
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
                font_family = font;
                font_size = 10;
                gradients = false;
                render_titles = true;
                scrolling = true;
              };

              insert_after_current = true;
              focus_removed_window = true;
            };

            misc = {
              vrr = 1;
              mouse_move_enables_dpms = true;
              key_press_enables_dpms = true;
              animate_mouse_windowdragging = true;
              animate_manual_resizes = true;
              enable_swallow = true;
              no_direct_scanout = false;
              cursor_zoom_factor = 1;
              force_default_wallpaper = 0;
              disable_hyprland_logo = true;
              disable_splash_rendering = true;
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

            windowrulev2 = [ "group new,class:${firefoxRegexp}" ] ++ noComposite
              ++ noBlurAndOpaque ++ workspaceAssigns ++ monitorAssigns;

            workspace = [ "7,monitor:${monitor1}" ];

            layerrule = [ "blur,fuzzel" ];

            bind = [
              "${mod},RETURN,exec,${pkgs.alacritty}/bin/alacritty"
              ''${mod}SHIFT,e,exec,${pkgs.emacs}/bin/emacsclient -c -a=""''
              "${mod}SHIFT,c,exec,${reload}"
              "${mod},f,fullscreen"
              "${mod}SHIFT,q,killactive,"
              "${mod}SHIFT,m,exit,"
              "${mod},y,togglefloating,"
              "${mod},p,pseudo,"
              "${mod},t,exec,${pkgs.swaynotificationcenter}/bin/swaync-client -t"
              "${mod}SHIFT,b,pin"
              ",Print,exec,${screenshotScript}/bin/screenshot"
              ''${mod},C,exec,"${toggleSinkScript}/bin/toggle-sink"''
              "${mod}SHIFT,o,movecurrentworkspacetomonitor,${monitor2}"
              "${mod}SHIFT,p,movecurrentworkspacetomonitor,${monitor1}"
              "${mod},minus,togglespecialworkspace"
              "${mod}SHIFT,minus,movetoworkspace,special"
              "${mod},mouse_down,workspace,e+1"
              "${mod},mouse_up,workspace,e-1"
              "${mod},g,togglegroup"
              "${mod}SHIFT,g,changegroupactive,f"
              "${mod}SHIFT,f,changegroupactive,g"
            ] ++ builtins.concatMap (num:
              let workspaceNum = "${toString num}";
              in [
                "${mod},${workspaceNum},workspace,${workspaceNum}"
                "${mod}SHIFT,${workspaceNum},movetoworkspace,${workspaceNum}"
              ]) (lists.range 0 9);

            bindm =
              [ "${mod},mouse:272,movewindow" "${mod},mouse:273,resizewindow" ];

            bindr = [
              "${mod},d,exec,${pkgs.procps}/bin/pkill fuzzel || ${pkgs.fuzzel}/bin/fuzzel"
              "${mod},ESCAPE,exec,${pkgs.procps}/bin/pkill fuzzel || ${powermenuScript}/bin/powermenu"
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
                ,XF86AudioMute,exec,${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && "${checkMuteScript}/bin/check-mute" "-s"''
              ''
                ,XF86AudioMicMute,exec,${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle && "${checkMuteScript}/bin/check-mute" "-m"''

            ];

            bindel = [
              '',XF86MonBrightnessUp,exec,${wobScript}/bin/wob "-b" "-i5"''
              '',XF86MonBrightnessDown,exec,${wobScript}/bin/wob "-b" "-d5"''
              '',XF86AudioRaiseVolume,exec,${wobScript}/bin/wob "-v" "-i5"''
              '',XF86AudioLowerVolume,exec,${wobScript}/bin/wob "-v" "-d5"''
              ",XF86AudioNext,exec,${pkgs.playerctl}/bin/playerctl next"
              ",XF86AudioPrev,exec,${pkgs.playerctl}/bin/playerctl previous"
              ",XF86AudioPlay,exec,${pkgs.playerctl}/bin/playerctl play-pause"
              ",XF86AudioStop,exec,${pkgs.playerctl}/bin/playerctl stop"
            ];
          };
        };
      };
    };
  };
}
