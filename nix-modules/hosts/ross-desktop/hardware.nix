{
  configurations.nixos.ross-desktop.module =
    { lib, pkgs, ... }:
    {
      nixpkgs.hostPlatform = "x86_64-linux";
      hardware = {
        enableRedistributableFirmware = true;
        cpu.amd.updateMicrocode = true;
        amdgpu.initrd.enable = true;

        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages = with pkgs; [
            vulkan-loader
            vulkan-validation-layers
            vulkan-extension-layer
          ];
        };
      };

      powerManagement = {
        enable = true;
        cpuFreqGovernor = "performance";
      };
    };
}
