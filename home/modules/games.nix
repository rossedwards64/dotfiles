{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.games;
in {
  options.modules.games = { enable = mkEnableOption "games"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      dxvk_2
      fceux
      gamescope
      glxinfo
      lutris
      mupen64plus
      protontricks
      steam
      vkd3d-proton
      vulkan-tools
      zsnes2
    ];
  };
}
