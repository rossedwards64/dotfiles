{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        environment.systemPackages = with pkgs; [
          winetricks
        ];
      };

    homeManager.base =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          ani-cli
          caprine
          cutter
          gimp
          kdePackages.ark
          libreoffice
          qbittorrent
          thunar
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
        ];

        programs = {
          pandoc.enable = true;
          nyxt.enable = true;
          mpv.enable = true;

          vesktop = {
            enable = true;
            settings = {
              appBadge = true;
              arRPC = true;
              checkUpdates = false;
              minimizeToTray = false;
              splashTheming = true;
              hardwareAcceleration = true;
              discordBranch = "stable";
            };
          };
        };
      };
  };
}
