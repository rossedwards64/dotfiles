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
  };

  modules = {
    thinkpad.enable = true;
    kde.enable = true;
  };

  hardware.opengl.extraPackages = [ pkgs.intel-vaapi-driver ];

  services = {
    xserver.xkb.layout = "gb";
    tlp.settings.NATACPI_ENABLE = 1;
  };

  networking.hostName = "ross-thinkpad-x230";

  system.stateVersion = "23.11";
}
