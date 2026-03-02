{ lib, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      boot = {
        tmp.cleanOnBoot = true;

        loader = {
          efi = {
            canTouchEfiVariables = true;
            efiSysMountPoint = "/boot";
          };

          grub = {
            enable = true;
            useOSProber = true;
            efiSupport = true;
            device = "nodev";

            theme =
              lib.mkForce
              <|
                pkgs.fetchFromGitHub {
                  owner = "catppuccin";
                  repo = "grub";
                  rev = "88f6124757331fd3a37c8a69473021389b7663ad";
                  sha256 = "sha256-e8XFWebd/GyX44WQI06Cx6sOduCZc5z7/YhweVQGMGY=";
                }
                + "/src/catppuccin-mocha-grub-theme";

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
        };

        binfmt.registrations.DOSWin = {
          recognitionType = "magic";
          offset = null;
          magicOrExtension = "MZ";
          mask = null;
          interpreter = "${lib.getExe pkgs.wineWow64Packages.waylandFull}";
          preserveArgvZero = false;
          openBinary = false;
          matchCredentials = false;
          fixBinary = false;
        };
      };
    };

}
