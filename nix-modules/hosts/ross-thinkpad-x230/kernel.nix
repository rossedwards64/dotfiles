{
  configurations.nixos.ross-thinkpad-x230.module = {
    boot = {
      extraModprobeConfig = "options thinkpad_acpi fan_control=1";
      kernelModules = [ "kvm-intel" ];
      initrd.availableKernelModules = [
        "xhci_pci"
        "ehci_pci"
        "ahci"
        "usb_storage"
        "sd_mod"
        "sdhci_pci"
      ];
    };
  };
}
