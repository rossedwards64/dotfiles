{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    let
      down = config.flake.meta.windowManager.down;
      right = config.flake.meta.windowManager.right;
      left = config.flake.meta.windowManager.left;
      up = config.flake.meta.windowManager.up;
      columnBindings =
        lib.attrsets.concatMapAttrs
          (key: direction: {
            "Mod+${key}".action."focus-column-${direction}" = [ ];
            "Mod+Shift+${key}".action."move-column-${direction}" = [ ];
          })
          {
            ${left} = "left";
            ${right} = "right";
            Left = "left";
            Right = "right";
          };

      windowBindings =
        lib.attrsets.concatMapAttrs
          (key: direction: {
            "Mod+${key}".action."focus-window-${direction}" = [ ];
            "Mod+Shift+${key}".action."move-window-${direction}" = [ ];
          })
          {
            ${up} = "up";
            ${down} = "down";
            Up = "up";
            Down = "down";
          };

      scrollBindings = lib.attrsets.mergeAttrsList [
        (lib.attrsets.concatMapAttrs
          (scroll: direction: {
            "Mod+${scroll}" = {
              cooldown-ms = 150;
              action."focus-workspace-${direction}" = [ ];
            };

            "Mod+Ctrl+${scroll}" = {
              cooldown-ms = 150;
              action."move-column-to-workspace-${direction}" = [ ];
            };
          })
          {
            WheelScrollDown = "down";
            WheelScrollUp = "up";
          }
        )
        (lib.attrsets.concatMapAttrs
          (scroll: direction: {
            "Mod+Shift+${scroll}".action."focus-column-${direction}" = [ ];
            "Mod+Ctrl+Shift+${scroll}".action."move-column-${direction}" = [ ];
          })
          {
            WheelScrollDown = "right";
            WheelScrollUp = "left";
          }
        )
      ];

      monitorBindings =
        lib.attrsets.concatMapAttrs
          (key: direction: {
            "Mod+Ctrl+${key}".action."focus-monitor-${direction}" = [ ];
            "Mod+Ctrl+Shift+${key}".action."move-column-to-monitor-${direction}" = [ ];
          })
          {
            ${down} = "down";
            ${left} = "left";
            ${right} = "right";
            ${up} = "up";
            down = "down";
            left = "left";
            right = "right";
            up = "up";
          };

      workspaceBindings =
        (
          lib.attrsets.mergeAttrsList
          <| map (num: {
            "Mod+${toString num}".action.focus-workspace = num;
            "Mod+Shift+${toString num}".action.move-column-to-workspace = num;
          })
          <| lib.lists.range 0 9
        )
        // (lib.attrsets.concatMapAttrs
          (key: direction: {
            "Mod+${key}".action."focus-workspace-${direction}" = [ ];
            "Mod+Shift+${key}".action."move-column-to-workspace-${direction}" = [ ];
            "Mod+Ctrl+${key}".action."move-workspace-${direction}" = [ ];
          })
          {
            i = "up";
            page_down = "down";
            page_up = "up";
            u = "down";
          }
        );

      widthBindings =
        lib.attrsets.concatMapAttrs
          (key: op: {
            "Mod+${key}".action.set-column-width = "${op}10%";
            "Mod+${key}".action.set-window-height = "${op}10%";
          })
          {
            minus = "-";
            equal = "+";
          };

      consumeOrExpelBindings =
        lib.attrsets.concatMapAttrs
          (key: direction: {
            "Mod+${key}".action."consume-or-expel-window-${direction}" = [ ];
          })
          {
            bracketleft = "left";
            bracketright = "right";
          };
    in
    {
      programs.niri.settings.binds = lib.attrsets.mergeAttrsList [
        columnBindings
        consumeOrExpelBindings
        monitorBindings
        scrollBindings
        windowBindings
        workspaceBindings
        {
          "Mod+c".action.spawn = "${lib.getExe config.flake.scripts.toggleSink}";
          "Mod+Comma".action.consume-window-into-column = [ ];
          "Mod+Ctrl+f".action.expand-column-to-available-width = [ ];
          "Mod+Ctrl+r".action.reset-window-height = [ ];
          "Mod+d".action.spawn-sh = "${lib.getExe' pkgs.procps "pkill"} fuzzel || ${lib.getExe pkgs.fuzzel}";
          "Mod+Escape".action.spawn-sh =
            "${lib.getExe' pkgs.procps "pkill"} fuzzel || ${lib.getExe config.flake.scripts.powerMenu}";
          "Mod+f".action.maximize-column = [ ];
          #"Mod+M".action.maximize-window-to-edges = [ ];
          "Mod+o".action.toggle-overview = [ ];
          "Mod+Period".action.expel-window-from-column = [ ];
          "Mod+r".action.switch-preset-column-width = [ ];
          "Mod+Return".action.spawn = "${lib.getExe pkgs.alacritty}";
          "Mod+Shift+f".action.fullscreen-window = [ ];
          "Mod+Shift+g".action.spawn-sh = "${lib.getExe' config.flake.pkgs.emacs "emacsclient"} -c -a=''";
          "Mod+Shift+q" = {
            repeat = false;
            action.close-window = [ ];
          };
          "Mod+Shift+r".action.switch-preset-window-height = [ ];
          "Mod+Shift+Return".action.spawn = "${lib.getExe pkgs.librewolf}";
          "Mod+Shift+Slash".action.show-hotkey-overlay = [ ];
          "Mod+Shift+Space".action.switch-focus-between-floating-and-tiling = [ ];
          "Mod+Shift+x".action.center-visible-columns = [ ];
          "Mod+Space".action.toggle-window-floating = [ ];
          "Mod+t".action.spawn-sh = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -t -sw";
          "Mod+w".action.toggle-column-tabbed-display = [ ];
          "Mod+x".action.center-column = [ ];
          "Print".action.spawn = "${lib.getExe config.flake.scripts.screenshot}";

          XF86AudioLowerVolume = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.runWob} -t volume -v 5-";
          };

          XF86AudioMicMute = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.toggleMute} -m";
          };

          XF86AudioMute = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.toggleMute} -s";
          };

          XF86AudioNext = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe pkgs.playerctl} next";
          };

          XF86AudioPlay = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe pkgs.playerctl} play-pause";
          };

          XF86AudioPrev = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe pkgs.playerctl} previous";
          };

          XF86AudioRaiseVolume = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.runWob} -t volume -v 5+";
          };

          XF86MonBrightnessDown = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.runWob} -t brightness -v 5-";
          };

          XF86MonBrightnessUp = {
            allow-when-locked = true;
            action.spawn-sh = "${lib.getExe config.flake.scripts.runWob} -t brightness -v 5+";
          };
        }
      ];
    };
}
