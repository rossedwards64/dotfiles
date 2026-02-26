{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        environment.systemPackages = with pkgs; [
          flatpak
          gnome-software
          neovim
          rose-pine-icon-theme
          wayland
          winetricks
          wl-clipboard
          xwayland
        ];

        services = {
          xserver = {
            enable = true;
            xkb = {
              variant = "";
              options = "ctrl:nocaps";
            };
          };

          jackett.enable = true;
        };

        xdg.portal = {
          enable = true;
          wlr.enable = true;
          config.common.default = "gtk";
        };
      };

    homeManager.base =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          ani-cli
          kdePackages.ark
          caprine
          gimp
          hledger
          imv
          iosevka
          vesktop
          libreoffice
          mpv
          neovim
          nyxt
          pandoc
          qbittorrent
          vlc
          thunar
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
          zathura
        ];

        programs.obs-studio = {
          enable = true;
          plugins = with pkgs.obs-studio-plugins; [
            obs-backgroundremoval
            obs-composite-blur
            obs-vkcapture
            wlrobs
          ];
        };
      };
  };
}
