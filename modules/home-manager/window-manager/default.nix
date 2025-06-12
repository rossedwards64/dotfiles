{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.window-manager;
in
{
  imports = [
    ./sway
    ./wm-programs
  ];

  options.modules.window-manager = {
    enable = mkEnableOption "window-manager";
  };

  config = mkIf cfg.enable {
    modules = {
      sway.enable = true;
      wm-programs.enable = true;
    };
  };
}
