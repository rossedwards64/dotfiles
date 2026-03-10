{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    let
      inherit (config.flake.meta) font;
      height = 32;
      hdmi = config.flake.meta.monitors.hdmi.name;
      dp1 = config.flake.meta.monitors.dp1.name;
      dp2 = config.flake.meta.monitors.dp2.name;
      laptopScreen = config.flake.meta.monitors.laptopScreen.name;
      layer = "bottom";

      makeDisk = disk: path: {
          name = "disk#${disk}";
          config = {
            format = "󰋊 ${lib.toUpper disk}: {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "${path}";
            interval = 300;
          };
        };

      makeDiskNoLabel = disk: path: {
          name = "disk#${disk}";
          config = {
            format = "󰋊 {used} / {total}";
            tooltip-format = "{used} / {total} used";
            path = "${path}";
            interval = 300;
          };
        };

      modules = {
        builtin = rec {
          clock = {
            name = "clock";
            config = {
              interval = 1;
              format = "<b>{:%a %d-%m-%y %H:%M:%S}</b>";
              format-alt = "<b>{:%A, %d of %B, %Y %H:%M:%S}</b>";
              tooltip-format = ''
                <big>{:%Y %B}</big>
                <tt><small>{calendar}</small></tt>'';
              calendar = {
                mode = "year";
                mode-mon-col = 3;
                weeks-pos = "right";
                on-scroll = 1;
                on-click-right = "mode";
                format = {
                  months = "<span color='#ffead3'><b>{}</b></span>";
                  days = "<span color='#ecc6d9'><b>{}</b></span>";
                  weeks = "<span color='#99ffdd'><b>W{}</b></span>";
                  weekdays = "<span color='#ffcc66'><b>{}</b></span>";
                  today = "<span color='#ff6699'><b><u>{}</u></b></span>";
                };
              };
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

          cpuNoLabel = lib.attrsets.overrideExisting cpu {
              config.format = "󰘚 {usage}% {avg_frequency}GHz";
            };

          memory = {
            name = "memory";
            config = {
              format = " RAM: {used:0.1f}G / {total:0.1f}G";
              tooltip-format = "{used:0.1f}G / {total:0.1f}G used";
            };
          };

          memoryNoLabel = lib.attrsets.overrideExisting memory {
              config.format = " {used:0.1f}G / {total:0.1f}G";
            };

          temperature = {
            name = "temperature";
            config = {
              critical-threshold = 70;
              format = "{icon} {temperatureC}°C";
              format-icons = [
                ""
                ""
                ""
                ""
                ""
              ];
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
                default = [
                  ""
                  ""
                ];
              };
              on-click = "${lib.getExe pkgs.pavucontrol}";
            };
          };

          disk = {
            root = makeDisk "root" "/";
            hdd = makeDisk "hdd" "/HDD";
            ssd = makeDisk "ssd" "/SSD";
            ssd2 = makeDisk "ssd2" "/SSD2";
          };

          diskNoLabel = {
            root = makeDiskNoLabel "root" "/";
            hdd = makeDiskNoLabel "hdd" "/HDD";
            ssd = makeDiskNoLabel "ssd" "/SSD";
            ssd2 = makeDiskNoLabel "ssd2" "/SSD2";
          };

          taskbar = {
            name = "wlr/taskbar";
            config = {
              format = "{icon}";
              icon-size = 24;
              icon-theme = "rose-pine-moon";
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
                on-click = "${lib.getExe' pkgs.sway "swaymsg"} kill";
              };
            };

            mode = {
              name = "sway/mode";
              config = {
                format = ''<span style="italic">{}</span>'';
              };
            };

            scratchpad = {
              name = "sway/scratchpad";
              config = {
                format = "{icon} {count}";
                format-icons = [
                  "󱘔"
                  "󱘒"
                ];
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
              on-scroll-down = "${lib.getExe pkgs.brightnessctl} set 5%-";
              on-scroll-up = "${lib.getExe pkgs.brightnessctl} set 5%+";
              format = "{icon} {percent}%";
              format-icons = [
                "󰃚"
                "󰃛"
                "󰃜"
                "󰃝"
                "󰃞"
                "󰃟"
                "󰃠"
              ];
              on-click = "${lib.getExe pkgs.wdisplays}";
            };
          };

          tray = {
            name = "tray";
            config = {
              icon-size = 24;
              spacing = 10;
            };
          };

          bluetooth = {
            name = "bluetooth";
            config = {
              interval = 10;
              controller = "ross-thinkpad-x230";
              format-device-preference = [ "ross-thinkpad-x230" ];
              format = " {controller_alias}";
              format-connected = " {device_alias}";
              format-disabled = " disabled";
              format-off = " off";
              format-on = " on";
              format-connected-battery = " {device_alias} ({device_battery_percentage}%)";
              tooltip-format = "{controller_alias}\\t{controller_address}";
              tooltip-format-connected = "{controller_alias}\\t{controller_address}\\n\\n{device_enumerate}";
              tooltip-format-enumerate-connected = "{device_alias}\\t{device_address}";
            };
          };

          battery = {
            name = "battery#bat1";
            config = {
              bat = "BAT0";
              adapter = "AC";
              interval = 10;
              full-at = 100;
              states = {
                "full" = 100;
                "good" = 99;
                "warning" = 30;
                "critical" = 20;
                "empty" = 15;
              };
              format-time = "{H}:{M}";
              format = "{icon}  {capacity}% [{time}]";
              format-charging = " {capacity}% [{time}]";
              format-plugged = " {capacity}% [{time}]";
              format-empty = "Empty";
              format-full = "Full [{time}]";
              format-icons = [
                ""
                ""
                ""
                ""
                ""
              ];
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
              on-click = "${lib.getExe pkgs.fuzzel}";
            };
          };

          cpuTemp = {
            name = "custom/cpu-temp";
            config = {
              exec = "${lib.getExe config.flake.scripts.cpuTemp}";
              format = " {}";
              interval = 1;
            };
          };

          gpuPercent = {
            name = "custom/gpu-percent";
            config = {
              exec = "${lib.getExe config.flake.scripts.gpuPercent}";
              format = "󰘚 GPU {}";
              interval = 1;
            };
          };

          gpuMem = {
            name = "custom/gpu-mem";
            config = {
              exec = "${lib.getExe config.flake.scripts.gpuMem}";
              format = "{}";
              interval = 1;
            };
          };

          gpuTemp = {
            name = "custom/gpu-temp";
            config = {
              exec = "${lib.getExe config.flake.scripts.gpuTemp}";
              format = " {}";
              interval = 1;
            };
          };

          power = {
            name = "custom/power";
            config = {
              tooltip = false;
              format = "󰐥";
              on-click = "${lib.getExe config.flake.scripts.powerMenu}";
            };
          };

          weather = {
            name = "custom/weather";
            config = {
              return-type = "json";
              exec = "sh ${lib.getExe config.flake.scripts.weather}";
              interval = 300;
              on-click = "${lib.getExe pkgs.librewolf} https://wttr.in";
            };
          };

          spotify = {
            name = "custom/spotify";
            config = {
              interval = 1;
              return-type = "json";
              exec = "${lib.getExe config.flake.scripts.currentMedia}";
              exec-if =
                let
                  pgrepCommand = lib.getExe' pkgs.procps "pgrep";
                in
                lib.intersperse "||" (
                  builtins.map (p: "${pgrepCommand} ${p}") [
                    "spotify"
                    "spotify_player"
                    "spot"
                    "ncspot"
                  ]
                );
              escape = true;
            };
          };

          notification = {
            name = "custom/notification";
            config = {
              tooltip = false;
              format = "{icon}";
              format-icons = {
                notification = "󰂚<span foreground='red'><sup> </sup></span>";
                none = "󰂚 ";
                dnd-notification = "󰂛<span foreground='red'><sup> </sup></span>";
                dnd-none = "󰂛 ";
                inhibited-notification = "󰂚<span foreground='red'><sup> </sup></span>";
                inhibited-none = "󰂚 ";
                dnd-inhibited-notification = "󰂛<span foreground='red'><sup> </sup></span>";
                dnd-inhibited-none = "󰂛 ";
              };
              return-type = "json";
              exec-if = "which ${lib.getExe' pkgs.swaynotificationcenter "swaync-client"}";
              exec = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -swb";
              on-click = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -t -sw";
              on-click-right = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -d -sw";
              escape = true;
            };
          };

          fanSpeed = {
            name = "custom/fan-speed";
            config = {
              format = "󰈐 {}";
              interval = 1;
              exec = "${lib.getExe config.flake.scripts.fanSpeed}";
            };
          };
        };
      };

      allModules =
        with modules;
        lib.mergeAttrsList (
          map (module: { "${module.name}" = module.config; }) [
            builtin.backlight
            builtin.battery
            builtin.clock
            builtin.cpu
            builtin.disk.hdd
            builtin.disk.root
            builtin.disk.ssd
            builtin.disk.ssd2
            builtin.diskNoLabel.root
            builtin.idleInhibitor
            builtin.memory
            builtin.network.disconnected
            builtin.network.ethernet
            builtin.network.wifi
            builtin.pulseaudio
            builtin.sway.mode
            builtin.sway.scratchpad
            builtin.sway.window
            builtin.taskbar
            builtin.temperature
            builtin.tray
            builtin.wireplumber
            custom.cpuTemp
            custom.fanSpeed
            custom.gpuMem
            custom.gpuPercent
            custom.gpuTemp
            custom.launcher
            custom.notification
            custom.power
            custom.separator
            custom.spotify
            custom.weather
          ]
        );

      allModulesNoLabels =
        with modules;
        lib.mergeAttrsList (
          map (module: { "${module.name}" = module.config; }) [
            builtin.backlight
            builtin.battery
            builtin.clock
            builtin.cpuNoLabel
            builtin.diskNoLabel.root
            builtin.idleInhibitor
            builtin.memoryNoLabel
            builtin.network.disconnected
            builtin.network.ethernet
            builtin.network.wifi
            builtin.pulseaudio
            builtin.sway.mode
            builtin.sway.scratchpad
            builtin.sway.window
            builtin.taskbar
            builtin.temperature
            builtin.tray
            builtin.wireplumber
            custom.cpuTemp
            custom.fanSpeed
            custom.gpuMem
            custom.gpuPercent
            custom.gpuTemp
            custom.launcher
            custom.notification
            custom.power
            custom.separator
            custom.spotify
            custom.weather
          ]
        );

      makeBar = moduleSet: position: output: modules-left: modules-center: modules-right:
        {
          inherit
            height
            layer
            position
            output
            modules-left
            modules-center
            modules-right
            ;
        }
        // moduleSet;

      makeBarWithLabels = position: output: modules-left: modules-center: modules-right:
        makeBar allModules position output modules-left modules-center modules-right;

      makeBarNoLabels = position: output: modules-left: modules-center: modules-right:
        makeBar allModulesNoLabels position output modules-left modules-center modules-right;
    in
    {
      programs.waybar = with modules; {
        enable = true;

        systemd.enable = true;

        settings = {
          topbar-hdmi = makeBarWithLabels "top" hdmi
              [
                "${custom.launcher.name}"
                "${custom.separator.name}"
                "${builtin.memory.name}"
                "${builtin.cpu.name}"
                "${custom.cpuTemp.name}"
                "${custom.gpuPercent.name}"
                "${custom.gpuMem.name}"
                "${custom.gpuTemp.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.clock.name}" ]
              [
                "${custom.separator.name}"
                "${builtin.network.disconnected.name}"
                "${builtin.network.wifi.name}"
                "${builtin.network.ethernet.name}"
                "${builtin.network.disconnected.name}"
                "${custom.separator.name}"
                "${custom.power.name}"
              ];
          bottombar-hdmi = makeBarWithLabels "bottom" hdmi
              [
                "${builtin.tray.name}"
                "${custom.separator.name}"
                "${builtin.backlight.name}"
                "${builtin.wireplumber.name}"
                "${builtin.idleInhibitor.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.taskbar.name}" ]
              [
                "${custom.separator.name}"
                "${builtin.sway.scratchpad.name}"
                "${builtin.sway.window.name}"
                "${custom.separator.name}"
              ];

          topbar-dp1 = makeBarWithLabels "top" dp1
              [
                "${custom.launcher.name}"
                "${custom.separator.name}"
                "${builtin.disk.root.name}"
                "${builtin.disk.ssd.name}"
                "${builtin.disk.ssd2.name}"
                "${builtin.disk.hdd.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.clock.name}" ]
              [
                "${custom.separator.name}"
                "${custom.spotify.name}"
                "${custom.weather.name}"
                "${custom.separator.name}"
                "${custom.power.name}"
              ];

          bottombar-dp1 = makeBarWithLabels "bottom" dp1
              [
                "${builtin.tray.name}"
                "${custom.separator.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.taskbar.name}" ]
              [
                "${custom.separator.name}"
                "${builtin.sway.scratchpad.name}"
                "${builtin.sway.window.name}"
                "${custom.separator.name}"
              ];

          topbar-dp2 = makeBarNoLabels "top" dp2
              [
                "${custom.launcher.name}"
                "${custom.separator.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.clock.name}" ]
              [
                "${custom.separator.name}"
                "${custom.separator.name}"
                "${custom.power.name}"
              ];

          bottombar-dp2 = makeBarNoLabels "bottom" dp2
              [
                "${builtin.tray.name}"
                "${custom.separator.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.taskbar.name}" ]
              [
                "${custom.separator.name}"
                "${builtin.sway.scratchpad.name}"
                "${builtin.sway.window.name}"
                "${custom.separator.name}"
              ];

          topbar-laptop = makeBarNoLabels "top" laptopScreen
              [
                "${custom.launcher.name}"
                "${custom.separator.name}"
                "${builtin.diskNoLabel.root.name}"
                "${builtin.memoryNoLabel.name}"
                "${builtin.cpuNoLabel.name}"
                "${builtin.temperature.name}"
                "${custom.fanSpeed.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.clock.name}" ]
              [
                "${custom.separator.name}"
                "${builtin.network.wifi.name}"
                "${builtin.network.ethernet.name}"
                "${builtin.network.disconnected.name}"
                "${builtin.bluetooth.name}"
                "${custom.separator.name}"
                "${custom.weather.name}"
                "${custom.separator.name}"
                "${custom.power.name}"
              ];

          bottombar-laptop = makeBarNoLabels "bottom" laptopScreen
              [
                "${builtin.tray.name}"
                "${custom.separator.name}"
                "${custom.notification.name}"
                "${custom.separator.name}"
                "${builtin.battery.name}"
                "${builtin.backlight.name}"
                "${builtin.wireplumber.name}"
                "${builtin.idleInhibitor.name}"
                "${custom.separator.name}"
              ]
              [ "${builtin.taskbar.name}" ]
              [
                "${custom.separator.name}"
                "${custom.spotify.name}"
                "${custom.separator.name}"
                "${builtin.sway.scratchpad.name}"
                "${builtin.sway.window.name}"
                "${custom.separator.name}"
              ];
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
              font-family: "${font}";
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

          .topbar-hdmi {
              border-bottom: 3px solid @overlay2;
          }

          .bottombar-hdmi {
              border-top: 3px solid @overlay2;
          }

          .topbar-dp {
              border-bottom: 3px solid @overlay2;
          }

          .bottombar-dp {
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
          #battery.bat1,
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
          #window,
          #language,
          #submap,
          #custom-cpu-temp,
          #custom-gpu-temp,
          #custom-gpu-percent,
          #custom-gpu-mem,
          #custom-spotify,
          #custom-notification,
          #bluetooth {
              padding: 0 5px;
              margin: 0 0px;
          }

          @keyframes blink {
              to {
                  background-color: @subtext1;
                  color: @crust;
              }
          }

          #battery.bat1.critical:not(.charging) {
              background-color: @red;
              animation-name: blink;
              animation-duration: 0.5s;
              animation-timing-function: linear;
              animation-iteration-count: infinite;
              animation-direction: alternate;
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

          #custom-notification {
              font-family: "${font}";
          }
        '';
      };
    };
}
