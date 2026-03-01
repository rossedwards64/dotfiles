{
  configurations.nixos.ross-thinkpad-x230.module = {
    boot.loader.grub.extraConfig = "GRUB_GFXMODE=1366x768,auto";
  };
}
