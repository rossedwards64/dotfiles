{ config, ... }:
{
  configurations.nixos.ross-desktop.module =
    { pkgs, ... }:
    {
      nixpkgs.hostPlatform = "x86_64-linux";
      hardware = {
        enableRedistributableFirmware = true;
        cpu.amd.updateMicrocode = true;
        amdgpu.initrd.enable = true;

        graphics.extraPackages = with pkgs; [
          vulkan-loader
          vulkan-validation-layers
          vulkan-extension-layer
        ];
      };

      powerManagement = {
        enable = true;
        cpuFreqGovernor = "performance";
      };
    };
}
