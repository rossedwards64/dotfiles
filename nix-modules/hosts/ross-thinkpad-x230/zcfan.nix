{
  configurations.nixos.ross-thinkpad-x230.module =
    { pkgs, ... }:
    {
      environment = {
        systemPackages = [ pkgs.zcfan ];

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

      systemd.services = {
        zcfan = {
          enable = true;
          description = "Zero-configuration fan control for ThinkPad.";
          serviceConfig = {
            ExecStart = "${pkgs.zcfan}/bin/zcfan";
            Restart = "on-failure";
            RestartSec = "500ms";
            MemoryDenyWriteExecute = "yes";
            NoNewPrivileges = "yes";
            ProtectControlGroups = "yes";
            RestrictAddressFamilies = "";
            RestrictRealtime = "yes";
            TimeoutStopSec = 2;
            StartLimitBurst = 2;
            StartLimitIntervalSec = 600;
          };
          wantedBy = [ "default.target" ];
        };
      };
    };
}
