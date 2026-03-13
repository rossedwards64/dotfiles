{ lib, config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.monitors) laptopScreen;
    in
    {
      wayland.windowManager.sway.config.output.${laptopScreen.name} = {
        inherit (laptopScreen) res scale;
        bg = "${laptopScreen.bg} fill";
      };

      programs.niri.settings = {
        outputs = with config.flake; {
          ${laptopScreen.name} =
            let
              toInt = lib.attrsets.mapAttrs (
                name: value:
                let
                  intValue = lib.strings.toInt value;
                in
                if (name == "refresh") then intValue + 0.0 else intValue
              );
              mode = toInt <| meta.lib.monitors.getRes laptopScreen;
              position = toInt <| meta.lib.monitors.getPos laptopScreen;
              scale = lib.strings.toInt laptopScreen.scale;
            in
            {
              inherit mode position scale;
            };
        };

        spawn-at-startup = lib.mkAfter [
          {
            command = [
              "${lib.getExe pkgs.swaybg}"
              "-o"
              "${laptopScreen.name}"
              "-i"
              "${laptopScreen.bg}"
            ];
          }
        ];
      };
    };
}
