{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.window-manager;
in {
  imports = [ ./sway ./hyprland ./wm-programs ];

  options.modules.window-manager = {
    enable = mkEnableOption "window-manager";
  };

  config = mkIf cfg.enable {
    modules = {
      sway.enable = true;
      hyprland.enable = true;
      wm-programs.enable = true;
    };
  };
}
