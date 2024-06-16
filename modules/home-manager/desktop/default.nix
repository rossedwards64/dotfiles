{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.desktop;
in {
  options.modules.desktop = { enable = mkEnableOption "desktop"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ani-cli
      armcord
      gimp
      imv
      iosevka
      libreoffice
      mpv
      neovim
      nyxt
      pandoc
      qbittorrent
      texliveFull
      vlc
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
}
