{ config, lib, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.config = {
        startup = lib.mkAfter [
          { command = "${pkgs.flatpak}/bin/flatpak run io.freetubeapp.FreeTube"; }
          { command = "${pkgs.itch}/bin/itch"; }
          { command = "${pkgs.qbittorrent}/bin/qbittorrent"; }
          { command = "${pkgs.steam}/bin/steam"; }
          { command = "${pkgs.vesktop}/bin/vesktop"; }
        ];
      };
    };
}
