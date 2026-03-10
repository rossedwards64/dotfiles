{ lib, config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.monitors) hdmi dp1 dp2;
      allMonitorsButLaptopScreen = (
        lib.attrsets.filterAttrs (name: _: name != "laptopScreen") config.flake.meta.monitors
      );
    in
    {
      wayland.windowManager.sway.config = {
        output =
          allMonitorsButLaptopScreen
          |> lib.attrsets.concatMapAttrs (
            _: monitor: {
              ${monitor.name} = {
                inherit (monitor) res scale pos;
                bg = "${monitor.bg} fill";
              };
            }
          );

        workspaceOutputAssign = [
          {
            workspace = "1";
            output = hdmi.name;
          }
          {
            workspace = "2";
            output = dp1.name;
          }
          {
            workspace = "3";
            output = dp1.name;
          }
          {
            workspace = "4";
            output = dp2.name;
          }
          {
            workspace = "5";
            output = dp2.name;
          }
          {
            workspace = "6";
            output = dp2.name;
          }
          {
            workspace = "7";
            output = hdmi.name;
          }
        ];
      };

      programs.niri.settings = {
        outputs =
          with config.flake;
          allMonitorsButLaptopScreen
          |> lib.attrsets.concatMapAttrs (
            _: monitor:
            let
              toNumber = lib.attrsets.mapAttrs (
                name: value:
                let
                  intValue = lib.strings.toInt value;
                in
                # convert the refresh rate to a float
                if (name == "refresh") then
                  intValue
                  + (
                    if (monitor.name == meta.monitors.dp1.name) then
                      0.001
                    else if (monitor.name == meta.monitors.dp2.name) then
                      0.025
                    else
                      0.0
                  )
                else
                  intValue
              );
              mode = toNumber <| meta.lib.monitors.getRes monitor;
              position = toNumber <| meta.lib.monitors.getPos monitor;
              scale = lib.strings.toInt monitor.scale;
            in
            {
              ${monitor.name} = {
                inherit mode position scale;
              };
            }
          );

        spawn-at-startup =
          lib.mkAfter
          <| builtins.map (monitor: {
            command = [
              "${lib.getExe pkgs.swaybg}"
              "-o"
              monitor.name
              "-i"
              monitor.bg
            ];
          }) allMonitorsButLaptopScreen;
      };
    };
}
