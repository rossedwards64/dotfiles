{config,...}:{
  configurations.nixos.ross-desktop.module = {pkgs,...}:{
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      extraModulePackages = [ config.boot.kernelPackages.gcadapter-oc-kmod ];

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
