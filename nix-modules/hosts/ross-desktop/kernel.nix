{ inputs, ... }:
{
  configurations.nixos.ross-desktop.module =
    { pkgs, ... }:
    {
      boot = {
        extraModulePackages = [
          inputs.nix-cachyos-kernel.legacyPackages.${pkgs.stdenv.hostPlatform.system}.linuxPackages-cachyos-latest.gcadapter-oc-kmod
        ];

        kernelModules = [
          "kvm-amd"
          "amdgpu"
          "gcadapter_oc"
          "it87"
        ];

        initrd.availableKernelModules = [
          "nvme"
          "xhci_pci"
          "ahci"
          "usbhid"
          "usb_storage"
          "sd_mod"
        ];
      };
    };
}
