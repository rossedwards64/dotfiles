{
  config,
  inputs,
  lib,
  ...
}:
{
  configurations.nixos.ross-thinkpad-x230.module =
    { pkgs, ... }:
    {
      nixpkgs.hostPlatform = "x86_64-linux";
      imports = [ inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x230 ];

      hardware = {
        enableRedistributableFirmware = true;
        cpu.intel.updateMicrocode = true;
        graphics.extraPackages = [ pkgs.intel-vaapi-driver ];

        bluetooth = {
          enable = true;
          powerOnBoot = true;
        };
      };
    };
}
