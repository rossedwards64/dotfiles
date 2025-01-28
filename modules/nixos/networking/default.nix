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

      firewall = {
        allowedTCPPorts = [
          20
          21
        ];
        connectionTrackingModules = [ "ftp" ];
      };
    };

    services = {
      avahi = {
        enable = true;
        nssmdns4 = true;
      };

      vsftpd = {
        enable = true;
        allowWriteableChroot = true;
        anonymousUser = true;
        anonymousUserNoPassword = true;
        anonymousMkdirEnable = true;
        localUsers = true;
        writeEnable = true;
      };
    };
  };
}
