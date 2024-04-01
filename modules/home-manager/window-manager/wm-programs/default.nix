{ lib, config, ... }:
with lib;
let cfg = config.modules.wm-programs;
in {
  imports = [ ./dunst ./rofi ./swayidle ./swaylock ./waybar ./wob ];

  options.modules.wm-programs = { enable = mkEnableOption "wm-programs"; };

  config = mkIf cfg.enable {
    modules = {
      dunst.enable = true;
      rofi.enable = true;
      swayidle.enable = true;
      swaylock.enable = true;
      waybar.enable = true;
      wob.enable = true;
    };
  };
}
