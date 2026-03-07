{ config, lib, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.config = {
        startup = lib.mkAfter [
          { command = "${lib.getExe pkgs.flatpak} run io.freetubeapp.FreeTube"; }
          { command = "${lib.getExe pkgs.itch}"; }
          { command = "${lib.getExe pkgs.qbittorrent}"; }
          { command = "${lib.getExe pkgs.steam}"; }
          { command = "${lib.getExe pkgs.vesktop}"; }
        ];
      };
    };
}
