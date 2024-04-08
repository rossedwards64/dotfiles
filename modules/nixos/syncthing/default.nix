{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.syncthing;
  home = "/home/ross";
  documents = "${home}/Documents";
in {
  options.modules.syncthing = { enable = mkEnableOption "syncthing"; };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      user = "ross";
      dataDir = "/home/ross";
      overrideDevices = true;
      overrideFolders = true;

      settings = {
        devices = {
          "ross-desktop" = {
            id =
              "4NVAFSD-7XR3LOQ-IYYFCO6-7IJAW4H-EXZM5DL-GL5AHT3-CNKDYQW-QWIQ7AP";
            name = "Desktop";
            autoAcceptFolders = true;
          };
          "ross-thinkpad-x230" = {
            id =
              "4M2JQRE-HGWHKMB-SOMBU3O-LFYSFDA-H3PXN2B-MMTJAAN-PPDGZPJ-Q46FEQN";
            name = "Thinkpad-X230";
            autoAcceptFolders = true;
          };
          "ross-thinkpad-x200" = {
            id = "";
            name = "Thinkpad-X200";
            autoAcceptFolders = true;
          };
          "ross-phone" = {
            id =
              "QGJKORY-OW4KYSG-OJVOE6V-I5OOE2A-4MHBM34-QUQRN6F-USEKHQN-OCTZNQS";
            name = "Phone";
            autoAcceptFolders = true;
          };
        };

        folders = {
          "Org Files" = { path = "${documents}/org"; };
          "Books" = { path = "${documents}/books"; };
          "Papers" = { path = "${documents}/papers"; };
          "Manuals" = { path = "${documents}/manuals"; };
          "Pictures" = { path = "${home}/Pictures/camera"; };
        };
        options = {
          urAccepted = -1;
          localAnnounceEnabled = true;
        };
      };
    };
  };
}
