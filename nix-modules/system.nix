{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        environment = {
          systemPackages = with pkgs; [
            just
            bc
            cachix
            jdk
            killall
            libnotify
            nix-output-monitor
            nvd
            openssl
            openssl.dev
            openssl.out
            stow
            wget
          ];
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

          nh = {
            enable = true;
            clean = {
              enable = true;
              extraArgs = "--keep-since 3d --keep 2";
            };
          };
        };

        console = {
          useXkbConfig = true;
        };

        services = {
          accounts-daemon.enable = true;
          flatpak.enable = true;
          openssh.enable = true;
          power-profiles-daemon.enable = false; # conflicts with TLP.
          udev.packages = with pkgs; [
            platformio-core.udev
            openocd
          ];

          pipewire = {
            enable = true;
            audio.enable = true;
            alsa.enable = true;
            alsa.support32Bit = true;
            pulse.enable = true;
            wireplumber.enable = true;

            lowLatency = {
              enable = true;
              quantum = 64;
              rate = 48000;
            };
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
          graphics.enable = true;

          bluetooth = {
            enable = true;
            powerOnBoot = true;
          };
        };
      };

    homeManager.base =
      { pkgs, config, ... }:
      {
        home.packages = with pkgs; [
          btop
          dust
          ffmpeg
          fzf
          gcr
          nix-health
          nix-info
          nixd
          nixfmt
          p7zip
          rar
          ripgrep
          rlwrap
          rsync
          tokei
          tree-sitter
          unzip
        ];

        xdg.enable = true;
        programs = {
          git = {
            enable = true;
            settings.user = {
              email = "redwards64@hotmail.com";
              name = "Ross Edwards";
            };
          };

          gpg = {
            enable = true;
            homedir = "${config.xdg.dataHome}/gnupg";
          };

          bat = {
            enable = true;
            extraPackages = with pkgs.bat-extras; [
              batdiff
              batman
              batgrep
              batwatch
            ];
          };

          tealdeer = {
            enable = true;

            settings = {
              display = {
                compact = true;
                nuse_pager = true;
              };

              updates = {
                auto_update = true;
              };
            };
          };

          fd = {
            enable = true;
            hidden = true;
          };
        };

        services = {
          gnome-keyring = {
            enable = true;
            components = [
              "pkcs11"
              "secrets"
              "ssh"
            ];
          };

          gpg-agent = {
            enable = true;
            enableZshIntegration = true;
            pinentry.package = pkgs.pinentry-qt;
          };
        };
      };
  };
}
