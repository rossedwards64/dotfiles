{
  configurations.nixos.ross-desktop.module =
    { lib, pkgs, ... }:
    {
      hardware = {
        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages = with pkgs; [
            vulkan-loader
            vulkan-validation-layers
            vulkan-extension-layer
          ];
        };

        cpu.amd.updateMicrocode = lib.mkDefault true;
      };

      powerManagement = {
        enable = true;
        cpuFreqGovernor = "performance";
      };
    };
}
