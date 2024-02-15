{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
      allowed-users = [ "ross" ];
    };
  };

  boot = {
    tmp.cleanOnBoot = true;

    extraModprobeConfig = ''
      options thinkpad_acpi fan_control=1                       
    '';

    loader.grub = {
      enable = true;
      device = "/dev/sda";
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
        GRUB_GFXMODE=1280x720,auto
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

  networking = {
    hostName = "ross-thinkpad-x200";
    useDHCP = false;
    networkmanager.enable = true;
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

  services = {
    xserver = {
      enable = true;

      displayManager = {
        defaultSession = "plasmawayland";
        sddm.enable = true;
      };

      desktopManager = { plasma5.enable = true; };

      xkb = {
        layout = "gb";
        variant = "";
        options = "ctrl:nocaps";
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };

  console = { useXkbConfig = true; };

  sound.enable = true;

  hardware = {
    pulseaudio.enable = false;

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = [ pkgs.intel-vaapi-driver ];
    };
  };

  security.rtkit.enable = true;

  users.users.ross = {
    isNormalUser = true;
    description = "Ross Edwards";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    useDefaultShell = true;
  };

  nixpkgs.config.allowUnfree = true;

  environment = {
    systemPackages = with pkgs; [
      autoconf
      cargo
      clang
      cmake
      flatpak
      gcc
      git
      gnome.gnome-software
      gnumake
      gnupg
      killall
      libtool
      neovim
      openssl
      openssl.dev
      openssl.out
      pinentry
      rustc
      rustup
      tlp
      wayland
      wget
      wineWowPackages.waylandFull
      winetricks
      xwayland
      zcfan
      zsh
    ];

    sessionVariables = rec {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";
      XDG_BIN_HOME = "$HOME/.local/bin";
      PATH = [ "${XDG_BIN_HOME}" ];
    };

    etc = {
      "zcfan.conf" = {
        text = ''
          max_temp 65
          med_temp 55
          low_temp 40
        '';
        mode = "644";
      };

      sbclrc = {
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
        mode = "644";
      };
    };
  };

  programs = {
    zsh.enable = true;
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "qt";
    };
  };

  services = {
    openssh.enable = true;
    flatpak.enable = true;
    power-profiles-daemon.enable = false;

    tlp = {
      enable = true;
      settings = {
        TLP_ENABLE = 1;
        TLP_WARN_LEVEL = 3;
        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;
        WIFI_PWR_ON_AC = "off";
        WIFI_PWR_ON_BAT = "on";
        DEVICES_TO_DISABLE_ON_STARTUP = "nfc wwan";
        DEVICES_TO_ENABLE_ON_STARTUP = "wifi";
        DEVICES_TO_ENABLE_ON_AC = "bluetooth nfc wifi wwan";
        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth nfc wwan";
        START_CHARGE_THRESH_BAT0 = 50;
        STOP_CHARGE_THRESH_BAT0 = 80;
        START_CHARGE_THRESH_BAT1 = 50;
        STOP_CHARGE_THRESH_BAT1 = 80;
        TPSMAPI_ENABLE = 1;
      };
    };
  };

  systemd.services = {
    zcfan = {
      enable = true;
      description = "Zero-configuration fan control for ThinkPad.";
      serviceConfig = {
        ExecStart = "${pkgs.zcfan}/bin/zcfan";
        Restart = "always";
        RestartSec = "500ms";
        MemoryDenyWriteExecute = "yes";
        NoNewPrivileges = "yes";
        ProtectControlGroups = "yes";
        RestrictAddressFamilies = "";
        RestrictRealtime = "yes";
        TimeoutStopSec = 2;
      };
      wantedBy = [ "default.target" ];
    };
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-kde ];
    config.common.default = "gtk";
  };

  system.stateVersion = "23.11";
}
