{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.networking;
in
{
  options.modules.networking = {
    enable = mkEnableOption "networking";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      nssmdns
    ];

    networking = {
      useDHCP = false;
      networkmanager.enable = true;
    };

    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };
  };
}
