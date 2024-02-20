{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.networking;
in {
  options.modules.networking = { enable = mkEnableOption "networking"; };

  config = mkIf cfg.enable {
    networking = {
      hostName = "ross-thinkpad-x200";
      useDHCP = false;
      networkmanager.enable = true;
    };
  };
}
