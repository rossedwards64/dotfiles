{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.system;
in {
  options.modules.system = { enable = mkEnableOption "system"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jdk17
      killall
      openssl
      openssl.dev
      openssl.out
      stow
      wget
    ];

    time.timeZone = "Europe/London";

    i18n = {
      defaultLocale = "en_GB.UTF-8";
      extraLocaleSettings = {
        LC_ADDRESS = "en_GB.UTF-8";
        LC_IDENTIFICATION = "en_GB.UTF-8";
        LC_MEASUREMENT = "en_GB.UTF-8";
        LC_MONETARY = "en_GB.UTF-8";
        LC_NAME = "en_GB.UTF-8";
        LC_NUMERIC = "en_GB.UTF-8";
        LC_PAPER = "en_GB.UTF-8";
        LC_TELEPHONE = "en_GB.UTF-8";
        LC_TIME = "en_GB.UTF-8";
      };
    };

    security.polkit.enable = true;

    programs = {
      dconf.enable = true;
      light.enable = true;
    };

    console = { useXkbConfig = true; };

    sound.enable = true;

    services = {
      accounts-daemon.enable = true;
      flatpak.enable = true;
      openssh.enable = true;
      power-profiles-daemon.enable = true;
      xserver.xkb.options = "ctrl:nocaps";
    };

    hardware = {
      pulseaudio.enable = false;

      opengl = {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
      };

      bluetooth = {
        enable = true;
        powerOnBoot = true;
      };
    };
  };
}
