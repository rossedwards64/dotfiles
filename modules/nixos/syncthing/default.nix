{
  lib,
  config,
  ...
}:
with lib;
let
  cfg = config.modules.syncthing;
  home = "/home/ross";
  documents = "${home}/Documents";
  desktop = "ross-desktop";
  thinkpad-x230 = "ross-thinkpad-x230";
  thinkpad-x200 = "ross-thinkpad-x200";
  thinkpad-x61t = "ross-thinkpad-x61t";
  phone = "ross-phone";
in
{
  options.modules.syncthing = {
    enable = mkEnableOption "syncthing";
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      user = "ross";
      dataDir = "/home/ross";
      overrideDevices = true;
      overrideFolders = true;

      settings = {
        options = {
          urAccepted = -1;
          localAnnounceEnabled = true;
        };

        devices =
          [
            {
              hostname = desktop;
              id = "KGSBO6J-ELN452E-UWO6FV3-PLEVU37-6QHVMGT-YEPYCUB-H6Z7JX7-OPH7AQF";
              name = "Desktop";
            }
            {
              hostname = thinkpad-x230;
              id = "4M2JQRE-HGWHKMB-SOMBU3O-LFYSFDA-H3PXN2B-MMTJAAN-PPDGZPJ-Q46FEQN";
              name = "Thinkpad-X230";
            }
            {
              hostname = thinkpad-x200;
              id = "D2UMJGH-DUNDDMX-LE7QF3S-XJG44ZN-EKY4HQZ-4F5R6S5-PYZ7TO6-FXV3TQ5";
              name = "Thinkpad-X200";
            }
            {
              hostname = thinkpad-x61t;
              id = "Y533GI4-3AZFQJQ-NV6YHHK-RVDEHQ2-Q5Y27BZ-SN6DSNK-N6SNRJ7-RJRS5AM";
              name = "Thinkpad-X61T";
            }
            {
              hostname = phone;
              id = "QGJKORY-OW4KYSG-OJVOE6V-I5OOE2A-4MHBM34-QUQRN6F-USEKHQN-OCTZNQS";
              name = "Phone";
            }
          ]
          |> map (
            {
              hostname,
              id,
              name,
            }:
            {
              ${hostname} = {
                inherit id name;
                autoAcceptFolders = false;
              };
            }
          )
          |> attrsets.mergeAttrsList;

        folders =
          [
            {
              name = "Org Files";
              path = "${documents}/org";
            }
            {
              name = "Reading";
              path = "${documents}/reading";
            }
            {
              name = "Pictures";
              path = "${home}/Pictures/camera";
            }
          ]
          |> map (
            { name, path }:
            {
              ${name} = {
                inherit path;
                enable = true;
                label = name;
                id = name;

                versioning = {
                  type = "simple";
                  params.keep = "10";
                };

                devices = [
                  desktop
                  thinkpad-x230
                  thinkpad-x200
                  thinkpad-x61t
                  phone
                ];
              };
            }
          )
          |> attrsets.mergeAttrsList;
      };
    };
  };
}
