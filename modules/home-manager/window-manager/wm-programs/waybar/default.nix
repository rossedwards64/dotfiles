{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.waybar;
  height = 32;
  hdmi = "HDMI-A-1";
  dp = "DP-1";

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

      disk = {
        root = {
          name = "disk#root";
          config = {
            format = "󰋊 ROOT: {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "/";
            interval = 300;
          };
        };

        hdd = {
          name = "disk#hdd";
          config = {
            format = "󰋊 HDD: {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "/HDD";
            interval = 300;
          };
        };

        ssd = {
          name = "disk#ssd";
          config = {
            format = "󰋊 SSD: {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "/SSD";
            interval = 300;
          };
        };

        ssd2 = {
          name = "disk#ssd2";
          config = {
            format = "󰋊 SSD2: {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "/SSD2";
            interval = 300;
          };
        };
      };

      taskbar = {
        name = "wlr/taskbar";
        config = {
          format = "{icon} {name}";
          icon-size = 24;
          tooltip = false;
          on-click = "activate";
          on-click-right = "close";
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
        name = "custom/cpu_temp";
        config = {
          exec = pkgs.writeShellScript "cpu_temp" ''
            temp=$(cat /sys/devices/pci0000:00/0000:00:18.3/hwmon/[[:print:]]*/temp1_input | bc)
            triple_digits=1000000

            if [[ "$temp" -ge "$triple_digits" ]]; then
                echo "\${"temp::-2"}°C"
            elif [[ "$temp" -lt "$triple_digits" ]]; then
                echo "\${"temp::-3"}°C"
            fi
          '';
          format = " {}";
          interval = 1;
        };
      };

      gpuPercent = {
        name = "custom/gpu_percent";
        config = {
          exec = pkgs.writeShellScript "gpu_percent" ''
            gpu_dir=/sys/class/drm/card1/device
            gpu_percent=$(cat $gpu_dir/gpu_busy_percent)
            echo "$gpu_percent%"
          '';
          format = "󰘚 GPU {}";
          interval = 1;
        };
      };

      gpuMem = {
        name = "custom/gpu_mem";
        config = {
          exec = pkgs.writeShellScript "gpu_mem" ''
            gpu_dir=/sys/class/drm/card1/device
            gpu_mem_total=$(cat $gpu_dir/mem_info_vram_total)
            gpu_mem_used=$(cat $gpu_dir/mem_info_vram_used)

            format_num() {
                echo "$(numfmt --to=iec-i --format "%-8.2f" $1)B" | tr -d ' ' | sed 's/G/ G/g' | sed 's/M/ M/g'
            }
            echo "$(format_num $gpu_mem_used) / $(format_num $gpu_mem_total)"
          '';
          format = "{}";
          interval = 1;
        };
      };

      gpuTemp = {
        name = "custom/gpu_temp";
        config = {
          exec = pkgs.writeShellScript "gpu_temp" ''
            temp=$(cat /sys/class/drm/card1/device/hwmon/[[:print:]]*/temp1_input | bc)
            triple_digits=100000

            if [[ "$temp" -ge "$triple_digits" ]]; then
                echo "\${"temp::-2"}°C"
            elif [[ "$temp" -lt "$triple_digits" ]]; then
                echo "\${"temp::-3"}°C"
            fi
          '';
          format = " {}";
          interval = 1;
        };
      };

      powerOff = {
        name = "custom/poweroff";
        config = {
          tooltip = false;
          format = "󰐥";
          on-click = pkgs.writeShellScript "poweroff" "\n";
        };
      };

      weather = {
        name = "custom/weather";
        config = {
          return-type = "json";
          exec = pkgs.writeShellScript "weather" "";
          interval = 300;
          on-click = "firefox https://wttr.in";
        };
      };

      mail = {
        name = "custom/mail";
        config = {
          tooltip = false;
          format = "󰐥 ";
          exec = pkgs.writeShellScript "poweroff" "";
          interval = 120;
        };
      };

      spotify = {
        name = "custom/spotify";
        config = {
          interval = 1;
          return-type = "json";
          exec = pkgs.writeSHellScript "spotify" "";
          exec-if = "pgrep spotify";
          escape = true;
        };
      };
    };
  };
in {
  options.modules.waybar = { enable = mkEnableOption "waybar"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ waybar ];

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
            "${custom.separator.name}"
            "${builtin.memory.name}"
            "${builtin.cpu.name}"
          ];
          modules-center = [ "${builtin.clock.name}" ];
          modules-right = [
            "${builtin.network.disconnected.name}"
            "${builtin.network.ethernet.name}"
            "${builtin.network.wifi.name}"
          ];

          "${builtin.clock.name}" = builtin.clock.config;
          "${builtin.cpu.name}" = builtin.cpu.config;
          "${builtin.memory.name}" = builtin.memory.config;
          "${builtin.network.disconnected.name}" =
            builtin.network.disconnected.config;
          "${builtin.network.ethernet.name}" = builtin.network.ethernet.config;
          "${builtin.network.wifi.name}" = builtin.network.wifi.config;
          "${builtin.pulseaudio.name}" = builtin.pulseaudio.config;
          "${builtin.sway.window.name}" = builtin.sway.window.config;
          "${builtin.temperature.name}" = builtin.temperature.config;
          "${custom.cpuTemp.name}" = custom.cpuTemp.config;
          "${custom.launcher.name}" = custom.launcher.config;
          "${custom.separator.name}" = custom.separator.config;
          "${custom.gpuTemp.name}" = custom.gpuTemp.config;
          "${custom.gpuMem.name}" = custom.gpuMem.config;
          "${custom.gpuPercent.name}" = custom.gpuPercent.config;
          "${builtin.tray.name}" = builtin.tray.config;
          "${builtin.gamemode.name}" = builtin.gamemode.config;
        };

        bottombar1 = {
          inherit height;
          layer = "top";
          position = "bottom";
          output = hdmi;
          modules-left = [
            "${builtin.tray.name}"
            "${builtin.gamemode.name}"
            "${builtin.hyprland.language.name}"
            "${custom.separator.name}"
            "${builtin.backlight.name}"
            "${builtin.pulseaudio.name}"
            "${builtin.idleInhibitor.name}"
            "${custom.separator.name}"
            "${builtin.hyprland.submap.name}"
          ];
          modules-center = [ "${builtin.taskbar.name}" ];
          modules-right =
            [ "${custom.separator.name}" "${builtin.hyprland.window.name}" ];

          "${builtin.clock.name}" = builtin.clock.config;
          "${builtin.cpu.name}" = builtin.cpu.config;
          "${builtin.memory.name}" = builtin.memory.config;
          "${builtin.network.disconnected.name}" =
            builtin.network.disconnected.config;
          "${builtin.network.ethernet.name}" = builtin.network.ethernet.config;
          "${builtin.network.wifi.name}" = builtin.network.wifi.config;
          "${builtin.pulseaudio.name}" = builtin.pulseaudio.config;
          "${builtin.sway.window.name}" = builtin.sway.window.config;
          "${builtin.temperature.name}" = builtin.temperature.config;
          "${custom.cpuTemp.name}" = custom.cpuTemp.config;
          "${custom.launcher.name}" = custom.launcher.config;
          "${custom.separator.name}" = custom.separator.config;
          "${custom.gpuTemp.name}" = custom.gpuTemp.config;
          "${custom.gpuMem.name}" = custom.gpuMem.config;
          "${custom.gpuPercent.name}" = custom.gpuPercent.config;
          "${builtin.tray.name}" = builtin.tray.config;
          "${builtin.gamemode.name}" = builtin.gamemode.config;
        };

        topbar2 = {
          inherit height;
          layer = "top";
          position = "top";
          output = dp;
          modules-left = [
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
            "${custom.mail.name}"
            "${custom.weather.name}"
            "${custom.separator.name}"
            "${custom.powerOff.name}"
          ];

          "${builtin.clock.name}" = builtin.clock.config;
          "${builtin.cpu.name}" = builtin.cpu.config;
          "${builtin.memory.name}" = builtin.memory.config;
          "${builtin.network.disconnected.name}" =
            builtin.network.disconnected.config;
          "${builtin.network.ethernet.name}" = builtin.network.ethernet.config;
          "${builtin.network.wifi.name}" = builtin.network.wifi.config;
          "${builtin.pulseaudio.name}" = builtin.pulseaudio.config;
          "${builtin.sway.window.name}" = builtin.sway.window.config;
          "${builtin.temperature.name}" = builtin.temperature.config;
          "${custom.cpuTemp.name}" = custom.cpuTemp.config;
          "${custom.launcher.name}" = custom.launcher.config;
          "${custom.separator.name}" = custom.separator.config;
          "${custom.gpuTemp.name}" = custom.gpuTemp.config;
          "${custom.gpuMem.name}" = custom.gpuMem.config;
          "${custom.gpuPercent.name}" = custom.gpuPercent.config;
          "${builtin.tray.name}" = builtin.tray.config;
          "${builtin.gamemode.name}" = builtin.gamemode.config;
        };

        bottombar2 = {
          inherit height;
          layer = "top";
          position = "bottom";
          output = dp;

          modules-left = [ ];
          modules-center = [ "${builtin.taskbar.name}" ];
          modules-right =
            [ "${custom.separator.name}" "${builtin.hyprland.window.name}" ];

          "${builtin.clock.name}" = builtin.clock.config;
          "${builtin.cpu.name}" = builtin.cpu.config;
          "${builtin.memory.name}" = builtin.memory.config;
          "${builtin.network.disconnected.name}" =
            builtin.network.disconnected.config;
          "${builtin.network.ethernet.name}" = builtin.network.ethernet.config;
          "${builtin.network.wifi.name}" = builtin.network.wifi.config;
          "${builtin.pulseaudio.name}" = builtin.pulseaudio.config;
          "${builtin.sway.window.name}" = builtin.sway.window.config;
          "${builtin.temperature.name}" = builtin.temperature.config;
          "${custom.cpuTemp.name}" = custom.cpuTemp.config;
          "${custom.launcher.name}" = custom.launcher.config;
          "${custom.separator.name}" = custom.separator.config;
          "${custom.gpuTemp.name}" = custom.gpuTemp.config;
          "${custom.gpuMem.name}" = custom.gpuMem.config;
          "${custom.gpuPercent.name}" = custom.gpuPercent.config;
          "${builtin.tray.name}" = builtin.tray.config;
          "${builtin.gamemode.name}" = builtin.gamemode.config;
        };
      };

      style = ''
        @define-color base   #303446;
        @define-color mantle #292c3c;
        @define-color crust  #232634;
        @define-color text     #c6d0f5;
        @define-color subtext0 #a5adce;
        @define-color subtext1 #b5bfe2;
        @define-color surface0 #414559;
        @define-color surface1 #51576d;
        @define-color surface2 #626880;
        @define-color overlay0 #737994;
        @define-color overlay1 #838ba7;
        @define-color overlay2 #949cbb;
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
        #custom-mail,
        #custom-poweroff,
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
        #custom-cpu_temp,
        #custom-gpu_temp,
        #custom-gpu_percent,
        #custom-gpu_mem,
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

        #custom-poweroff {
            margin: 0 5px 0 0;
        }
      '';
    };
  };
}
