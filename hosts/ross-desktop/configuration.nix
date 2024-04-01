{ config, lib, pkgs, specialArgs, ... }:

{
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

    initrd.kernelModules = [ "amdgpu" ];
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

  modules = { kde.enable = true; };

  services.xserver = {
    xkb.layout = "us";
    videoDrivers = [ "amdgpu" ];
  };

  networking.hostName = "ross-desktop";

  system.stateVersion = "23.11";
}
