{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    let
      inherit (config.flake.meta.windowManager) modifier;
    in
    { pkgs, ... }:
    {
      home.packages = [
        config.flake.scripts.screenshot
      ];

      wayland.windowManager.sway = {
        enable = true;
        checkConfig = true;
        xwayland = true;
        wrapperFeatures = {
          base = true;
          gtk = true;
        };

        systemd = {
          enable = true;
          xdgAutostart = true;
          variables = [ "--all" ];
        };

        swaynag = {
          enable = true;
        };

        config = {
          inherit modifier;
          inherit (config.flake.meta.windowManager)
            down
            right
            left
            up
            ;
          bindkeysToCode = false;
          defaultWorkspace = "workspace number 1";
          menu = "${lib.getExe pkgs.fuzzel}";
          terminal = "${lib.getExe pkgs.alacritty}";
          workspaceAutoBackAndForth = true;
          workspaceLayout = "tabbed";
          bars = [ ];
          window.border = 1;

          fonts = {
            names = [ config.flake.meta.font ];
            style = "Regular";
          };

          gaps = {
            inner = 5;
            outer = 5;
            smartBorders = "off";
            smartGaps = false;
          };

          floating = {
            inherit modifier;
            border = 1;
            criteria = [ ];
            titlebar = true;
          };

          focus = {
            followMouse = "always";
            mouseWarping = false;
            newWindow = "urgent";
          };
        };
      };
    };
}
