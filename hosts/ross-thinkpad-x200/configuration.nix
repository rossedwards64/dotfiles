{
  pkgs,
  ...
}:
let
  folderConfig = {
    enable = true;
    devices = [
      "ross-desktop"
      "ross-thinkpad-x230"
      "ross-phone"
    ];
  };
in
{
  imports = [ ./hardware-configuration.nix ];
  boot.loader.grub.device = "/dev/sda";
  hardware.graphics.extraPackages = [ pkgs.intel-vaapi-driver ];

  services = {
    xserver.xkb.layout = "gb";
    tlp.settings.TPSMAPI_ENABLE = 1;

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

  networking.hostName = "ross-thinkpad-x200";

  system.stateVersion = "23.11";
}
