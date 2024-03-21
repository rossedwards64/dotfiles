{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.services;
in {
  options.modules.services = { enable = mkEnableOption "services"; };

  config = mkIf cfg.enable {
    services = {
      openssh.enable = true;
      flatpak.enable = true;
      power-profiles-daemon.enable = false;
    };
  };
}
