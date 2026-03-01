{ config, lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    let
      modifier = config.flake.meta.windowManager.modifier;
      down = config.flake.meta.windowManager.down;
      right = config.flake.meta.windowManager.right;
      left = config.flake.meta.windowManager.left;
      up = config.flake.meta.windowManager.up;
      navigationBindings =
        lib.attrsets.concatMapAttrs
          (key: direction: {
            "${modifier}+${key}" = "focus ${direction}";
            "${modifier}+Shift+${key}" = "move ${direction}";
          })
          {
            ${left} = "left";
            ${right} = "right";
            ${up} = "up";
            ${down} = "down";
            left = "left";
            right = "right";
            up = "up";
            down = "down";
          };

      moveToWorkspaceBindings = lib.attrsets.mergeAttrsList (
        map (num: {
          "${modifier}+${toString num}" = "workspace number ${toString num}";
          "${modifier}+Ctrl+${toString num}" = "move container to workspace number ${toString num}";
          "${modifier}+Shift+${toString num}" =
            "move container to workspace number ${toString num}, workspace number ${toString num}";
        })
        <| lib.lists.range 0 9
      );
    in
    {
      wayland.windowManager.sway.config.keybindings =
        lib.attrsets.mergeAttrsList
        <| [
          navigationBindings
          moveToWorkspaceBindings
          {
            "${modifier}+Ctrl+space" = "sticky toggle";
            "${modifier}+Escape" =
              ''exec "${pkgs.procps}/bin/pkill fuzzel || ${config.flake.scripts.powerMenu}/bin/powermenu"'';
            "${modifier}+Minus" = "scratchpad show";
            "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
            "${modifier}+Shift+Return" = "exec ${pkgs.librewolf}/bin/librewolf";
            "${modifier}+Shift+b" = "border toggle";
            "${modifier}+Shift+c" = "reload";
            "${modifier}+Shift+g" = "exec ${config.flake.pkgs.emacs}/bin/emacsclient -c -a=''";
            "${modifier}+Shift+minus" = "move to scratchpad";
            "${modifier}+Shift+q" = "kill";
            "${modifier}+Shift+r" = ''mode "resize"'';
            "${modifier}+Shift+space" = "floating toggle";
            "${modifier}+Shift+v" = "${pkgs.myxer}/bin/myxer";
            "${modifier}+a" = "focus parent";
            "${modifier}+b" = "splitt";
            "${modifier}+c" = "exec ${config.flake.scripts.toggleSink}/bin/toggle-sink";
            "${modifier}+d" = ''exec "${pkgs.procps}/bin/pkill fuzzel || ${pkgs.fuzzel}/bin/fuzzel"'';
            "${modifier}+e" = "layout toggle all";
            "${modifier}+f" = "fullscreen";
            "${modifier}+g" = "splith";
            "${modifier}+r" = ''mode "default"'';
            "${modifier}+space" = "focus mode_toggle";
            "${modifier}+t" = "exec ${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
            "${modifier}+v" = "splitv";
            "${modifier}+w" = "layout default";
            "Print" = "exec ${config.flake.scripts.screenshot}/bin/screenshot";
          }
        ];
    };
}
