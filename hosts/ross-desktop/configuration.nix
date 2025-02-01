{
  config,
  pkgs,
  ...
}:
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
  imports = [ ./hardware-configuration.nix ];

  boot = {
    extraModulePackages = [ config.boot.kernelPackages.gcadapter-oc-kmod ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [
      "amdgpu"
      "gcadapter_oc"
      "it87"
    ];

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

  fileSystems = {
    "/HDD" = {
      device = "/dev/disk/by-uuid/992809b3-ca72-4c22-ae5b-41fd2f3967b2";
      fsType = "ext4";
    };

    "/SSD" = {
      device = "/dev/disk/by-uuid/d1055482-e9bc-4abc-90a9-53018bd30937";
      fsType = "ext4";
    };

    "/SSD2" = {
      device = "/dev/disk/by-uuid/280db991-0e21-444e-a497-073c021ddec7";
      fsType = "ext4";
    };
  };

  environment.systemPackages = with pkgs; [
    atlauncher
    #cemu
    dolphin-emu
    pcsx2
    rpcs3
    ryujinx
    xemu
  ];

  services = {
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

    udev.packages = [ pkgs.dolphin-emu ];
  };

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        vulkan-loader
        vulkan-validation-layers
        vulkan-extension-layer
      ];
    };

    cpu.amd.updateMicrocode = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "performance";
  };

  networking.hostName = "ross-desktop";

  system.stateVersion = "23.11";
}
