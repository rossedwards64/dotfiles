{ config, lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.windowManager)
        down
        right
        left
        up
        ;

      modifier = "Mod4";
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

      moveToWorkspaceBindings =
        lib.attrsets.mergeAttrsList
        <| map (num: {
          "${modifier}+${toString num}" = "workspace number ${toString num}";
          "${modifier}+Ctrl+${toString num}" = "move container to workspace number ${toString num}";
          "${modifier}+Shift+${toString num}" =
            "move container to workspace number ${toString num}, workspace number ${toString num}";
        })
        <| lib.lists.range 0 9;
    in
    {
      wayland.windowManager.sway.config.keybindings = lib.attrsets.mergeAttrsList [
        navigationBindings
        moveToWorkspaceBindings
        {
          "${modifier}+Ctrl+space" = "sticky toggle";
          "${modifier}+Escape" =
            ''exec "${lib.getExe' pkgs.procps "pkill"} fuzzel || ${lib.getExe config.flake.scripts.powerMenu}"'';
          "${modifier}+Minus" = "scratchpad show";
          "${modifier}+Return" = "exec ${lib.getExe pkgs.alacritty}";
          "${modifier}+Shift+Return" = "exec ${lib.getExe pkgs.librewolf}";
          "${modifier}+Shift+b" = "border toggle";
          "${modifier}+Shift+c" = "reload";
          "${modifier}+Shift+g" = "exec ${lib.getExe' config.flake.pkgs.emacs "emacsclient"} -c -a=''";
          "${modifier}+Shift+minus" = "move to scratchpad";
          "${modifier}+Shift+q" = "kill";
          "${modifier}+Shift+r" = ''mode "resize"'';
          "${modifier}+Shift+space" = "floating toggle";
          "${modifier}+Shift+v" = "${lib.getExe pkgs.myxer}";
          "${modifier}+a" = "focus parent";
          "${modifier}+b" = "splitt";
          "${modifier}+c" = "exec ${lib.getExe config.flake.scripts.toggleSink}";
          "${modifier}+d" = "exec ${lib.getExe' pkgs.procps "pkill"} fuzzel || ${lib.getExe pkgs.fuzzel}";
          "${modifier}+e" = "layout toggle all";
          "${modifier}+f" = "fullscreen";
          "${modifier}+g" = "splith";
          "${modifier}+r" = ''mode "default"'';
          "${modifier}+space" = "focus mode_toggle";
          "${modifier}+t" = "exec ${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -t -sw";
          "${modifier}+v" = "splitv";
          "${modifier}+w" = "layout default";
          "Print" = "exec ${lib.getExe config.flake.scripts.screenshot}";
        }
      ];
    };
}
