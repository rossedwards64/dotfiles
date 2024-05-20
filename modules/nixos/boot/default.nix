{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.boot;
in {
  options.modules.boot = { enable = mkEnableOption "boot"; };

  config = mkIf cfg.enable {
    boot = {
      tmp.cleanOnBoot = true;

      loader.grub = {
        enable = true;
        useOSProber = true;

        extraConfig = ''
          GRUB_CMDLINE_LINUX=""
          GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet acpi_enforce_resources=lax"
          GRUB_DEFAULT=0
          GRUB_GFXMODE=1920x1080,auto
          GRUB_GFXPAYLOAD_LINUX=keep
          GRUB_TERMINAL_INPUT="console"
          GRUB_TIMEOUT=5
          GRUB_TIMEOUT_STYLE=menu
        '';
      };

      binfmt = {
        registrations = {
          DOSWin = {
            recognitionType = "magic";
            offset = null;
            magicOrExtension = "MZ";
            mask = null;
            interpreter = "${pkgs.wineWowPackages.waylandFull}/bin/wine64";
            preserveArgvZero = false;
            openBinary = false;
            matchCredentials = false;
            fixBinary = false;
          };
        };
      };
    };
  };
}
