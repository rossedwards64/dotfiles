{ config, lib, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    let
      modifier = config.flake.meta.windowManager.modifier;
      hdmi = config.flake.meta.monitors.hdmi;
      dp1 = config.flake.meta.monitors.dp1;
      dp2 = config.flake.meta.monitors.dp2;
    in
    {
      wayland.windowManager.sway.config = {
        keybindings = lib.mkAfter (
          {
            i = hdmi.name;
            o = dp1.name;
            p = dp2.name;
          }
          |> lib.attrsets.concatMapAttrs (
            key: monitor: {
              "${modifier}+Shift+${key}" = "move workspace to output ${monitor}";
            }
          )
        );
      };
    };
}
