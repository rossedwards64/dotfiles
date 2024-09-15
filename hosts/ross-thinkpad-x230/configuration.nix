{
  pkgs,
  ...
}:
let
  folderConfig = {
    enable = true;
    devices = [
      "ross-desktop"
      "ross-thinkpad-x200"
      "ross-phone"
    ];
  };
in
{
  imports = [ ./hardware-configuration.nix ];

  nix = {
    settings = {
      auto-optimise-store = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  boot = {
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };

      grub = {
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  hardware.graphics.extraPackages = [ pkgs.intel-vaapi-driver ];

  services = {
    blueman.enable = true;
    xserver.xkb.layout = "gb";
    tlp.settings.NATACPI_ENABLE = 1;

    syncthing = {
      settings = {
        folders = {
          "Org Files" = folderConfig;
          "Books" = folderConfig;
          "Papers" = folderConfig;
          "Manuals" = folderConfig;
          "Pictures" = folderConfig;
        };
      };
    };
  };

  networking.hostName = "ross-thinkpad-x230";

  system.stateVersion = "23.11";
}
