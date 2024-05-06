{ config, lib, pkgs, specialArgs, ... }:
let
  folderConfig = {
    enable = true;
    devices = [ "ross-thinkpad-x230" "ross-thinkpad-x200" "ross-phone" ];
  };
in {
  imports = [ ./hardware-configuration.nix ];

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

  fileSystems."/HDD" = {
    label = "HDD";
    device = "/dev/disk/by-uuid/992809b3-ca72-4c22-ae5b-41fd2f3967b2";
    fsType = "ext4";
  };

  fileSystems."/SSD" = {
    label = "SSD";
    device = "/dev/disk/by-uuid/d1055482-e9bc-4abc-90a9-53018bd30937";
    fsType = "ext4";
  };

  fileSystems."/SSD2" = {
    label = "SSD2";
    device = "/dev/disk/by-uuid/280db991-0e21-444e-a497-073c021ddec7";
    fsType = "ext4";
  };

  environment.systemPackages = with pkgs; [
    # cemu
    ryujinx
    pcsx2
    rpcs3
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
          "Books" = folderConfig;
          "Papers" = folderConfig;
          "Manuals" = folderConfig;
          "Pictures" = folderConfig;
        };
      };
    };
  };

  hardware = {
    opengl = {
      extraPackages = with pkgs; [
        vulkan-loader
        vulkan-validation-layers
        vulkan-extension-layer
      ];
    };

    fancontrol = {
      enable = true;
      config = "";
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
