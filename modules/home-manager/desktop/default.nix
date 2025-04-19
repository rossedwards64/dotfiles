{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.desktop;
in
{
  options.modules.desktop = {
    enable = mkEnableOption "desktop";
  };

  config = mkIf cfg.enable {
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
      xfce.thunar
      xfce.thunar-archive-plugin
      xfce.thunar-media-tags-plugin
      xfce.thunar-volman
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
