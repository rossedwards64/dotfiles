{ lib, config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.monitors) hdmi dp1 dp2;
    in
    {
      wayland.windowManager.sway.config = {
        output =
          (lib.attrsets.filterAttrs (name: _: name != "laptopScreen") config.flake.meta.monitors)
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
          (lib.attrsets.filterAttrs (name: _: name != "laptopScreen") meta.monitors)
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

        spawn-at-startup = lib.mkAfter [
          {
            command = [
              "${lib.getExe pkgs.swaybg}"
              "-o"
              "HDMI-A-1"
              "-i"
              "${pkgs.fetchurl {
                url = "https://i.pinimg.com/originals/40/e9/da/40e9daa6982435261c840673b008b5dd.jpg";
                sha256 = "sha256-Q8ShPxMnk3TqivNXQ5wcC1fsRE6ISvCDxcEif605c5c=";
              }}"
            ];
          }
          {
            command = [
              "${lib.getExe pkgs.swaybg}"
              "-o"
              "DP-1"
              "-i"
              "${pkgs.fetchurl {
                url = "https://images.alphacoders.com/133/thumb-1920-1334857.png";
                sha256 = "sha256-1bJ4FSrY2G/UKK24w/+sZ5HwpnQgapX4dcRR/j15Jrk=";
              }}"
            ];
          }
          {
            command = [
              "${lib.getExe pkgs.swaybg}"
              "-o"
              "DP-2"
              "-i"
              "${pkgs.fetchurl {
                url = "https://images4.alphacoders.com/128/thumb-1920-1280154.jpg";
                sha256 = "sha256-QxC7Yju8dGZghLOi35ObE9rofONk6Mju+DA2IdvusAI=";
              }}"
            ];
          }
        ];
      };
    };
}
