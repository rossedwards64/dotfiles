{
  configurations.nixos.ross-thinkpad-x230.module =
    { pkgs, ... }:
    {
      environment.systemPackages = [ pkgs.tlp ];
      services = {
        power-profiles-daemon.enable = false;
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
            START_CHARGE_THRESH_BAT0 = 75;
            STOP_CHARGE_THRESH_BAT0 = 85;
            START_CHARGE_THRESH_BAT1 = 75;
            STOP_CHARGE_THRESH_BAT1 = 85;
          };
        };
      };
    };
}
