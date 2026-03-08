{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    let
      layout = "us";
      options = "ctrl:nocaps";
    in
    {
      wayland.windowManager.sway.config.input = {
        "12625:16386:ROYUAN_EPOMAKER_TH66" = {
          xkb_layout = layout;
          xkb_options = options;
        };
      };

      programs.niri.settings.input = {
        trackpoint.enable = false;
        keyboard.xkb = {
          inherit layout options;
        };
      };
    };
}
