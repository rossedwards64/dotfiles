{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.desktop;
in {
  options.modules.desktop = { enable = mkEnableOption "desktop"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ani-cli
      gimp
      iosevka
      libreoffice
      mpv
      ncspot
      neovim
      nyxt
      pandoc
      qbittorrent
      texliveFull
      vlc
      zathura
    ];
  };
}
