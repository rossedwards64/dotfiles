{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.desktop;
in {
  options.modules.desktop = { enable = mkEnableOption "desktop"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      firefox
      neovim
      nyxt
      syncthing
      syncthingtray
      gimp
      mpv
      ncspot
      vlc
      ani-cli
      libreoffice
      texliveFull
      zathura
      pandoc
    ];
  };
}
