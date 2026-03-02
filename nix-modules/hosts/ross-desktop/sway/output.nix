{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.monitors) hdmi dp1 dp2;
    in
    {
      wayland.windowManager.sway.config = {

        output = {
          ${hdmi.name} = {
            scale = "2";
            inherit (hdmi) res;
            pos = "0 0";
            bg = "${
              pkgs.fetchurl {
                url = "https://i.pinimg.com/originals/40/e9/da/40e9daa6982435261c840673b008b5dd.jpg";
                sha256 = "sha256-Q8ShPxMnk3TqivNXQ5wcC1fsRE6ISvCDxcEif605c5c=";
              }
            } fill";
          };

          ${dp1.name} = {
            scale = "1";
            inherit (dp1) res;
            pos = "1920 88";
            bg = "${
              pkgs.fetchurl {
                url = "https://images.alphacoders.com/133/thumb-1920-1334857.png";
                sha256 = "sha256-1bJ4FSrY2G/UKK24w/+sZ5HwpnQgapX4dcRR/j15Jrk=";
              }
            } fill";
          };

          ${dp2.name} = {
            scale = "1";
            inherit (dp2) res;
            pos = "3840 628";
            bg = "${
              pkgs.fetchurl {
                url = "https://images4.alphacoders.com/128/thumb-1920-1280154.jpg";
                sha256 = "sha256-QxC7Yju8dGZghLOi35ObE9rofONk6Mju+DA2IdvusAI=";
              }
            } fill";
          };
        };

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
    };
}
