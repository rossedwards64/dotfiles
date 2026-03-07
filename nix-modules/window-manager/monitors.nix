{ lib, ... }:
{
  flake.meta.lib.monitors = {
    getRes =
      monitor:
      let
        res = lib.strings.splitStringBy (
          prev: curr:
          builtins.elem curr [
            "x"
            "@"
          ]
        ) false monitor.res;
      in
      {
        width = builtins.head res;
        height = lib.lists.elemAt res 1;
        refresh = builtins.head <| lib.strings.splitString "Hz" <| lib.lists.elemAt res 2;
      };

    getPos =
      monitor:
      let
        pos = lib.strings.splitString " " monitor.pos;
      in
      {
        x = builtins.head pos;
        y = lib.lists.elemAt pos 1;
      };
  };

  flake.meta.monitors = {
    hdmi = {
      name = "HDMI-A-1";
      res = "3840x2160@60Hz";
      scale = "2";
      pos = "0 0";
    };

    dp1 = {
      name = "DP-1";
      res = "1920x1080@144Hz";
      scale = "1";
      pos = "1920 88";
    };

    dp2 = {
      name = "DP-2";
      res = "1280x1024@75Hz";
      scale = "1";
      pos = "3840 628";
    };

    laptopScreen = {
      name = "LVDS-1";
      res = "1366x768@60Hz";
      scale = "1";
      pos = "0 0";
    };
  };
}
