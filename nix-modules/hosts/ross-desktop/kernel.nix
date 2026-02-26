{
  configurations.nixos.ross-desktop.module =
    { pkgs, ... }:
    {
      boot = {
        extraModulePackages = [ pkgs.linuxKernel.packages.linux_6_18.gcadapter-oc-kmod ];

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
