let
  folderConfig = {
    enable = true;
    devices = [
      "ross-thinkpad-x230"
      "ross-thinkpad-x200"
      "ross-phone"
    ];
  };
in
{
  configurations.nixos.ross-desktop.module =
    { pkgs, ... }:
    {
      networking.hostName = "ross-desktop";
      system.stateVersion = "23.11";

      environment.systemPackages = with pkgs; [
        atlauncher
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

        syncthing = {
          settings = {
            folders = {
              "Org Files" = folderConfig;
              "Pictures" = folderConfig;
              "Reading" = folderConfig;
            };
          };
        };
      };
    };
}
