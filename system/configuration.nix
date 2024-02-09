# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };
  };

  # Bootloader.
  boot = {
    loader.grub = {
      enable = true;
      device = "/dev/sda";
      useOSProber = true;
      font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-regular.ttf";
    };

    extraModprobeConfig = ''
      options thinkpad_acpi fan_control=1                       
    '';
  };

  networking.hostName = "ross-thinkpad-x200"; # Define your hostname.
  networking.useDHCP = false;
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";

  i18n.extraLocaleSettings = {
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

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services.xserver.displayManager.defaultSession = "plasmawayland";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Configure keymap in X11
  services.xserver = {
    xkb = {
      layout = "gb";
      variant = "";
      options = "ctrl:nocaps";
    };
  };

  # Configure console keymap
  console = { useXkbConfig = true; };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ross = {
    isNormalUser = true;
    description = "Ross Edwards";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    useDefaultShell = true;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs = {
    zsh.enable = true;
    dconf.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "qt";
    };
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
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
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    config.common.default = "gtk";
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
