{ config, lib, pkgs, specialArgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.device = "/dev/sda";

  modules = {
    thinkpad.enable = true;
    kde.enable = true;
  };

  opengl.extraPackages = [ pkgs.intel-vaapi-driver ];

  services.xserver.xkb.layout = "gb";

  networking.hostName = "ross-thinkpad-x200";

  system.stateVersion = "23.11";
}
