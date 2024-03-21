{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.thinkpad;
in {
  options.modules.thinkpad = { enable = mkEnableOption "thinkpad"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ tlp zcfan ];

    boot.extraModprobeConfig = ''
      options thinkpad_acpi fan_control=1                       
    '';

    environment = {
      etc = {
        "zcfan.conf" = {
          text = ''
            max_temp 70
            med_temp 60
            low_temp 40
          '';
          mode = "644";
        };
      };
    };

    services = {
      tlp = {
        enable = true;
        settings = {
          TLP_ENABLE = 1;
          TLP_WARN_LEVEL = 3;
          CPU_BOOST_ON_AC = 1;
          CPU_BOOST_ON_BAT = 0;
          WIFI_PWR_ON_AC = "off";
          WIFI_PWR_ON_BAT = "on";
          DEVICES_TO_DISABLE_ON_STARTUP = "nfc wwan";
          DEVICES_TO_ENABLE_ON_STARTUP = "wifi";
          DEVICES_TO_ENABLE_ON_AC = "bluetooth nfc wifi wwan";
          DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth nfc wwan";
          START_CHARGE_THRESH_BAT0 = 50;
          STOP_CHARGE_THRESH_BAT0 = 80;
          START_CHARGE_THRESH_BAT1 = 50;
          STOP_CHARGE_THRESH_BAT1 = 80;
          TPSMAPI_ENABLE = 1;
        };
      };
    };

    systemd.services = {
      zcfan = {
        enable = true;
        description = "Zero-configuration fan control for ThinkPad.";
        serviceConfig = {
          ExecStart = "${pkgs.zcfan}/bin/zcfan";
          Restart = "always";
          RestartSec = "500ms";
          MemoryDenyWriteExecute = "yes";
          NoNewPrivileges = "yes";
          ProtectControlGroups = "yes";
          RestrictAddressFamilies = "";
          RestrictRealtime = "yes";
          TimeoutStopSec = 2;
        };
        wantedBy = [ "default.target" ];
      };
    };
  };
}
