{
  configurations.nixos.ross-desktop.module =
    { pkgs, ... }:
    {
      networking.hostName = "ross-desktop";
      system.stateVersion = "23.11";

      environment.systemPackages = with pkgs; [
        azahar
        #cemu
        #flycast
        dolphin-emu
        pcsx2
        rpcs3
        ryubing
        xemu
        lact
      ];

      stylix.fonts.sizes = {
        applications = 14;
        terminal = 12;
        desktop = 10;
        popups = 16;
      };

      systemd = {
        packages = [ pkgs.lact ];
        services.lactd.wantedBy = [ "multi-user.target" ];
      };

      services = {
        udev.packages = [ pkgs.dolphin-emu ];

        xserver = {
          xkb.layout = "us";
          videoDrivers = [ "modesetting" ];
        };
      };
    };
}
