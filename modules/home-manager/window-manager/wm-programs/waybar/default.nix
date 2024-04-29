{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.waybar;
  height = 32;
  hdmi = "HDMI-A-1";
  dp = "DP-1";

  cpuTempScript = import ./scripts/cpu-temp.nix { inherit pkgs; };
  gpuTempScript = import ./scripts/gpu-temp.nix { inherit pkgs; };
  gpuMemScript = import ./scripts/gpu-mem.nix { inherit pkgs; };
  gpuPercentScript = import ./scripts/gpu-percent.nix { inherit pkgs; };
  powerScript = import ./scripts/power.nix { inherit pkgs; };
  spotifyScript = import ./scripts/spotify.nix { inherit pkgs; };
  weatherScript = import ./scripts/weather.nix { inherit pkgs; };

  makeDisk = (disk: path: {
    name = "disk#${disk}";
    config = {
      format = "󰋊 ${lib.toUpper disk}: {used} / {total}";
      tooltip-format = "{used} / {total} used";
      path = "${path}";
      interval = 300;
    };
  });

  modules = {
    builtin = {
      clock = {
        name = "clock";
        config = {
          interval = 1;
          format = "<b>{:%a %d-%m-%y %H:%M:%S}</b>";
          format-alt = "<b>{:%A, %d of %B, %Y %H:%M:%S}</b>";
          tooltip-format = ''
            <big>{:%Y %B}</big>
            <tt><small>{calendar}</small></tt>'';
        };
      };

      cpu = {
        name = "cpu";
        config = {
          format = "󰘚 CPU: {usage}% {avg_frequency}GHz";
          tooltip = false;
          interval = 1;
        };
      };

      memory = {
        name = "memory";
        config = {
          format = " RAM: {used:0.1f}G / {total:0.1f}G";
          tooltip-format = "{used:0.1f}G / {total:0.1f}G used";
        };
      };

      temperature = {
        name = "temperature";
        config = {
          critical-threshold = 70;
          format = "{icon} {temperature}°C";
          format-icons = [ "" "" "" "" "" ];
        };
      };

      network = {
        disconnected = {
          name = "network#disconnected";
          config = {
            tooltip-format = "No connection";
            format-ethernet = "";
            format-wifi = "";
            format-linked = "";
            format-disconnected = "";
            on-click = "nm-connection-editor";
          };
        };

        ethernet = {
          name = "network#ethernet";
          config = {
            interface = "eno*";
            format-ethernet = "󰌗  {ifname}: {ipaddr}/{cidr}";
            format-wifi = "";
            format-linked = "";
            format-disconnected = "";
            tooltip-format = "{ifname}: {ipaddr}/{cidr}";
            on-click = "nm-connection-editor";
          };
        };

        wifi = {
          name = "network#wifi";
          config = {
            interface = "wlp*";
            format-ethernet = "";
            format-wifi = "  {essid} ({signalStrength}%)";
            format-linked = "";
            format-disconnected = "";
            tooltip-format = "{ifname}: {ipaddr}/{cidr}";
            on-click = "nm-connection-editor";
          };
        };
      };

      pulseaudio = {
        name = "pulseaudio";
        config = {
          scroll-step = 1;
          format = "{icon} {volume}%{format_source}";
          format-bluetooth = "{icon}󰂯 {volume}%{format_source}";
          format-bluetooth-muted = " {icon}󰂯 {format_source}";
          format-muted = " {format_source}";
          format-source = "󰍬 {volume}%";
          format-source-muted = "󰍭";
          format-icons = {
            headphone = "󰋋";
            phone = "󰏲";
            portable = "";
            car = "󰄋";
            default = [ ];
          };
          on-click = "alsamixer";
        };
      };

      wireplumber = {
        name = "wireplumber";
        config = {
          format = "{volume}% {icon}";
          format-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "󰂯";
            headset = "󰋎";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" ];
          };
          on-click = "{pkgs.helvum}/bin/helvum";
        };
      };

      disk = {
        root = makeDisk "root" "/";
        hdd = makeDisk "hdd" "/HDD";
        ssd = makeDisk "ssd" "/SSD";
        ssd2 = makeDisk "ssd2" "/SSD2";
      };

      taskbar = {
        name = "wlr/taskbar";
        config = {
          format = "{icon} {name}";
          icon-size = 32;
          icon-theme =
            [ "${pkgs.rose-pine-icon-theme}/share/icons/rose-pine-moon" ];
          tooltip = false;
          on-click = "activate";
          on-click-right = "close";
          sort-by-app-id = true;
        };
      };

      sway = {
        window = {
          name = "sway/window";
          config = {
            format = "{} 󱂬";
            on-click = "swaymsg kill";
          };
        };

        mode = {
          name = "sway/mode";
          config = { format = ''<span style="italic">{}</span>''; };
        };

        scratchpad = {
          name = "sway/scratchpad";
          config = {
            format = "{icon} {count}";
            format-icons = [ "󱘔" "󱘒" ];
            show-empty = true;
            tooltip = true;
            tooltip-format = "{app}: {title}";
          };
        };
      };

      hyprland = {
        window = {
          name = "hyprland/window";
          config = {
            format = "{}";
            separate-outputs = true;
          };
        };

        submap = {
          name = "hyprland/submap";
          config = {
            format = "Mode: {}";
            max-length = 8;
            tooltip = false;
          };
        };

        language = {
          name = "hyprland/language";
          config = {
            format = "{}";
            keyboard-name = "at-translated-set-2-keyboard";
          };
        };
      };

      idleInhibitor = {
        name = "idle_inhibitor";
        config = {
          format = "{icon}";
          format-icons = {
            activated = "󰈈";
            deactivated = "󰈉";
          };
        };
      };

      backlight = {
        name = "backlight";
        config = {
          device = "intel_backlight";
          interval = 1;
          on-scroll-down = "brightlight -pd 1";
          on-scroll-up = "brightlight -pi 1";
          format = "{icon} {percent}%";
          format-icons = [ "󰃚" "󰃛" "󰃜" "󰃝" "󰃞" "󰃟" "󰃠" ];
          on-click = "wdisplays";
        };
      };

      gamemode = {
        name = "gamemode";
        config = {
          format = "{glyph}";
          format-alt = "{glyph} {count}";
          glyph = "󰮂";
          hide-not-running = false;
          use-icon = true;
          icon-name = "input-gaming-symbolic";
          icon-spacing = 4;
          icon-size = 24;
          tooltip = true;
          tooltip-format = "Games running: {count}";
        };
      };

      tray = {
        name = "tray";
        config = {
          icon-size = 24;
          spacing = 10;
        };
      };
    };

    custom = {
      separator = {
        name = "custom/sep";
        config = {
          format = "|";
          interval = "once";
          tooltip = false;
        };
      };

      launcher = {
        name = "custom/launcher";
        config = {
          format = "";
          on-click = "$HOME/.config/rofi/scripts/launcher";
        };
      };

      cpuTemp = {
        name = "custom/cpu-temp";
        config = {
          exec = "${cpuTempScript}/bin/cpu-temp";
          format = " {}";
          interval = 1;
        };
      };

      gpuPercent = {
        name = "custom/gpu-percent";
        config = {
          exec = "${gpuPercentScript}/bin/gpu-percent";
          format = "󰘚 GPU {}";
          interval = 1;
        };
      };

      gpuMem = {
        name = "custom/gpu-mem";
        config = {
          exec = "${gpuMemScript}/bin/gpu-mem";
          format = "{}";
          interval = 1;
        };
      };

      gpuTemp = {
        name = "custom/gpu-temp";
        config = {
          exec = "${gpuTempScript}/bin/gpu-temp";
          format = " {}";
          interval = 1;
        };
      };

      power = {
        name = "custom/power";
        config = {
          tooltip = false;
          format = "󰐥";
          on-click = "${powerScript}/bin/power";
        };
      };

      weather = {
        name = "custom/weather";
        config = {
          return-type = "json";
          exec = "${weatherScript}/bin/weather";
          interval = 300;
          on-click = "${pkgs.firefox}/bin/firefox https://wttr.in";
        };
      };

      spotify = {
        name = "custom/spotify";
        config = {
          interval = 1;
          return-type = "json";
          exec = "${spotifyScript}/bin/spotify";
          exec-if =
            "${pkgs.procps}/bin/pgrep spotify || ${pkgs.procps}/bin/pgrep spotify_player || ${pkgs.procps}/bin/pgrep spot || ${pkgs.procps}/bin/pgrep ncspot";
          escape = true;
        };
      };
    };
  };

  allModules = with modules;
    lib.mergeAttrsList
    (builtins.map (module: { "${module.name}" = module.config; }) [
      builtin.clock
      builtin.cpu
      builtin.disk.hdd
      builtin.disk.root
      builtin.disk.ssd
      builtin.disk.ssd2
      builtin.gamemode
      builtin.memory
      builtin.network.disconnected
      builtin.network.ethernet
      builtin.network.wifi
      builtin.pulseaudio
      builtin.sway.window
      builtin.sway.mode
      builtin.sway.scratchpad
      builtin.temperature
      builtin.tray
      builtin.taskbar
      builtin.idleInhibitor
      builtin.wireplumber
      custom.cpuTemp
      custom.gpuMem
      custom.gpuPercent
      custom.gpuTemp
      custom.launcher
      custom.power
      custom.separator
      custom.spotify
      custom.weather
    ]);
in {
  options.modules.waybar = { enable = mkEnableOption "waybar"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ waybar procps ];

    programs.waybar = with modules; {
      enable = true;

      systemd.enable = true;

      settings = {
        topbar1 = {
          inherit height;
          layer = "top";
          position = "top";
          output = hdmi;

          modules-left = [
            "${custom.launcher.name}"
            "${custom.separator.name}"
            "${builtin.memory.name}"
            "${builtin.cpu.name}"
            "${custom.cpuTemp.name}"
            "${custom.gpuPercent.name}"
            "${custom.gpuMem.name}"
            "${custom.gpuTemp.name}"
            "${custom.separator.name}"
          ];
          modules-center = [ "${builtin.clock.name}" ];
          modules-right = [
            "${custom.separator.name}"
            "${builtin.network.disconnected.name}"
            "${builtin.network.wifi.name}"
            "${builtin.network.ethernet.name}"
            "${builtin.network.disconnected.name}"
            "${custom.separator.name}"
            "${custom.power.name}"
          ];
        } // allModules;

        bottombar1 = {
          inherit height;
          layer = "top";
          position = "bottom";
          output = hdmi;
          modules-left = [
            "${builtin.tray.name}"
            "${builtin.gamemode.name}"
            "${custom.separator.name}"
            "${builtin.backlight.name}"
            "${builtin.wireplumber.name}"
            "${builtin.idleInhibitor.name}"
            "${custom.separator.name}"
          ];
          modules-center = [ "${builtin.taskbar.name}" ];
          modules-center-right =
            [ "${custom.separator.name}" "${custom.separator.name}" ];
        } // allModules;

        topbar2 = {
          inherit height;
          layer = "top";
          position = "top";
          output = dp;
          modules-left = [
            "${custom.launcher.name}"
            "${custom.separator.name}"
            "${builtin.disk.root.name}"
            "${builtin.disk.ssd.name}"
            "${builtin.disk.ssd2.name}"
            "${builtin.disk.hdd.name}"
            "${custom.separator.name}"
          ];
          modules-center = [ "${builtin.clock.name}" ];
          modules-right = [
            "${custom.separator.name}"
            "${custom.spotify.name}"
            "${custom.weather.name}"
            "${custom.separator.name}"
            "${custom.power.name}"
          ];
        } // allModules;

        bottombar2 = {
          inherit height;
          layer = "top";
          position = "bottom";
          output = dp;

          modules-left =
            [ "${custom.separator.name}" "${custom.separator.name}" ];
          modules-center = [ "${builtin.taskbar.name}" ];
          modules-right = [
            "${custom.separator.name}"
            "${builtin.sway.scratchpad.name}"
            "${builtin.sway.window.name}"
            "${custom.separator.name}"
          ];
        } // allModules;
      };

      style = ''
        @define-color base      #303446;
        @define-color mantle    #292c3c;
        @define-color crust     #232634;
        @define-color text      #c6d0f5;
        @define-color subtext0  #a5adce;
        @define-color subtext1  #b5bfe2;
        @define-color surface0  #414559;
        @define-color surface1  #51576d;
        @define-color surface2  #626880;
        @define-color overlay0  #737994;
        @define-color overlay1  #838ba7;
        @define-color overlay2  #949cbb;
        @define-color blue      #8caaee;
        @define-color lavender  #babbf1;
        @define-color sapphire  #85c1dc;
        @define-color sky       #99d1db;
        @define-color teal      #81c8be;
        @define-color green     #a6d189;
        @define-color yellow    #e5c890;
        @define-color peach     #ef9f76;
        @define-color maroon    #ea999c;
        @define-color red       #e78284;
        @define-color mauve     #ca9ee6;
        @define-color pink      #f4b8e4;
        @define-color flamingo  #eebebe;
        @define-color rosewater #f2d5cf;

        * {
            border: none;
            border-radius: 0;
            font-family: "Iosevka NF";
            font-size: 13px;
            font-weight: normal;
            min-height: 24px;
            color: @text;
        }

        window#waybar {
            background: @mantle;
            background-color: @mantle;
            color: #C4C7C5;
            transition-property: background-color;
            transition-duration: .5s;
        }

        window#waybar.empty {
            color: @crust;
        }

        .topbar1 {
            border-bottom: 3px solid @overlay2;
        }

        .bottombar1 {
            border-top: 3px solid @overlay2;
        }

        .topbar2 {
            border-bottom: 3px solid @overlay2;
        }

        .bottombar2 {
            border-top: 3px solid @overlay2;
        }

        #workspaces button {
            padding: 0 5px 0 5px;
            background-color: transparent;
        }

        #workspaces button:hover {
            background: @crust;
            box-shadow: inset 0 3px @lavender;
        }

        #workspaces button.focused {
            background-color: @base;
            box-shadow: inset 0 3px @lavender;
        }

        #workspaces button.urgent {
            background-color: @red;
        }

        #mode {
            background-color: @mantle;
            border-top: 3px solid @lavender;
        }

        #backlight,
        #clock,
        #cpu,
        #custom-power,
        #custom-weather,
        #disk,
        #idle_inhibitor,
        #memory,
        #mode,
        #network.vpn,
        #network.wifi,
        #network.ethernet,
        #network.disconnected,
        #pulseaudio,
        #taskbar,
        #temperature,
        #tray,
        #gamemode,
        #window,
        #language,
        #submap,
        #custom-cpu-temp,
        #custom-gpu-temp,
        #custom-gpu-percent,
        #custom-gpu-mem,
        #custom-spotify,
        #custom-pacman {
            padding: 0 5px;
            margin: 0 0px;
        }

        @keyframes blink {
            to {
                background-color: @subtext1;
                color: @crust;
            }
        }

        #temperature.critical {
            background-color: @red;
        }

        #taskbar button:hover {
            background: @crust;
            box-shadow: inset 0 3px @lavender;
        }

        #taskbar button.active {
            background-color: @crust;
            box-shadow: inset 0 3px @lavender;
        }

        #custom-sep {
            color: @subtext1;
            padding: 0 5px;
        }

        #custom-launcher {
            color: #1794D2;
            font-size: 18px;
            padding: 0 5px;
        }

        #custom-power {
            margin: 0 5px;
        }
      '';
    };
  };
}
