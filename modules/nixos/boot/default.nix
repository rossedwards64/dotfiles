{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.boot;
in {
  options.modules.boot = { enable = mkEnableOption "boot"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      grub2_efi
      iosevka
      wineWowPackages.wayland
    ];

    boot = {
      tmp.cleanOnBoot = true;

      loader.grub = {
        enable = true;
        useOSProber = true;
        font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-regular.ttf";

        theme = pkgs.fetchFromGitHub ({
          owner = "catppuccin";
          repo = "grub";
          rev = "803c5df0e83aba61668777bb96d90ab8f6847106";
          sha256 = "sha256-/bSolCta8GCZ4lP0u5NVqYQ9Y3ZooYCNdTwORNvR7M0=";
        }) + "/src/catppuccin-mocha-grub-theme";

        extraConfig = ''
          GRUB_CMDLINE_LINUX=""
          GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet acpi_enforce_resources=lax"
          GRUB_DEFAULT=0
          GRUB_GFXMODE=1280x800,auto
          GRUB_GFXPAYLOAD_LINUX=keep
          GRUB_TERMINAL_INPUT=console
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
