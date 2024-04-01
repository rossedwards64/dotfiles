{ config, lib, pkgs, specialArgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.device = "/dev/sda";

  modules = {
    thinkpad.enable = true;
    kde.enable = true;
  };

  hardware.opengl.extraPackages = [ pkgs.intel-vaapi-driver ];

  services = { 
    xserver.xkb.layout = "gb"; 
    tlp.settings.TPSMAPI_ENABLE = 1;
  };

  networking.hostName = "ross-thinkpad-x200";

  system.stateVersion = "23.11";
}
