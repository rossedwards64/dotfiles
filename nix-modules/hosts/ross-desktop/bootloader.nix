{
  configurations.nixos.ross-desktop.module = {
    boot.loader.grub.extraConfig = ''
      GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet acpi_enforce_resources=lax"
      GRUB_GFXMODE=1920x1080,auto
    '';
  };
}
