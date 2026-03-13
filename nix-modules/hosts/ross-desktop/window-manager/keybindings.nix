{ config, lib, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    let
      inherit (config.flake.meta.windowManager) modifier;
      inherit (config.flake.meta.monitors) hdmi dp1 dp2;
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
