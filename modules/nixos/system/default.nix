{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.system;
in {
  options.modules.system = { enable = mkEnableOption "system"; };

  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        bc
        jdk
        killall
        libnotify
        nh
        nix-output-monitor
        nvd
        openssl
        openssl.dev
        openssl.out
        stow
        wget
      ];

      etc.sbclrc = {
        mode = "644";
        text = ''
          #+asdf (require :asdf)
          (ql:quickload :cffi :silent t)

          (pushnew (merge-pathnames ".nix-profile/lib/" (user-homedir-pathname))
                    cffi:*foreign-library-directories*)
          (pushnew (merge-pathnames "/run/current-system/sw/lib/" (user-homedir-pathname))
                    cffi:*foreign-library-directories*)

          (let ((default-init-file (funcall sb-ext:*userinit-pathname-function*)))
            (unless (or (null default-init-file)
                         (typep default-init-file 'stream)
                         (uiop:file-exists-p default-init-file))
              (setf sb-ext:*userinit-pathname-function*
                        (lambda () (uiop:xdg-config-home #P"sbcl/init.lisp")))))
        '';
      };
    };

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

    security = {
      polkit.enable = true;
      rtkit.enable = true;
    };

    programs = {
      dconf.enable = true;
      light.enable = true;
    };

    console = { useXkbConfig = true; };

    services = {
      accounts-daemon.enable = true;
      flatpak.enable = true;
      openssh.enable = true;
      power-profiles-daemon.enable = false; # conflicts with TLP.
      udev.packages = with pkgs; [
        android-udev-rules
        platformio-core.udev
        openocd
      ];

      pipewire = {
        enable = true;
        audio.enable = true;
        pulse.enable = true;
        wireplumber.enable = true;
      };
    };

    xdg = {
      icons.enable = true;
      menus.enable = true;
      sounds.enable = true;
      autostart.enable = true;

      portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = with pkgs; [
          xdg-desktop-portal-gnome
          xdg-desktop-portal-gtk
          xdg-desktop-portal-hyprland
        ];
      };
    };

    hardware = {
      opengl.enable = true;

      bluetooth = {
        enable = true;
        powerOnBoot = true;
      };
    };
  };
}
