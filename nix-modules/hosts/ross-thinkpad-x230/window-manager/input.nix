{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module =
    let
      layout = "gb";
      options = "ctrl:nocaps";
    in
    {
      programs.niri.settings.input.keyboard.xkb = {
        inherit layout options;
      };

      wayland.windowManager.sway.config.input = {
        "1:1:AT_Translated_Set_2_keyboard" = {
          xkb_layout = layout;
          xkb_options = options;
        };
      };
    };
}
